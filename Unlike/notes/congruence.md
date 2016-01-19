Example: perspective distortion of 9228 compared to 9227 (norm)

top-left    : (-51,  0)
top-right   : (-52, 10)
bottom-right: (-47, 10)
bottom-left : (-49, -2)

We also need an inverse transform formula.

- http://de.mathworks.com/help/vision/ref/blockmatching.html
- http://opencv-code.com/tutorials/automatic-perspective-correction-for-quadrilateral-objects/

Result: Product(IntPoint2D(134,-58),IntPoint2D(-122,-90),IntPoint2D(70,86),IntPoint2D(-22,-54))

--------------

Result: Product(IntPoint2D(99,37),IntPoint2D(45,37),IntPoint2D(74,100),IntPoint2D(100,98),0.108918909043577)
Result: Product(IntPoint2D(74,100),IntPoint2D(96,100),IntPoint2D(74,100),IntPoint2D(98,100),0.1292718797132856)

--------------

- http://www3.nd.edu/~kwb/ThomasKareemBowyerIGARSS_2012.pdf
- http://www.codeproject.com/Articles/95453/Automatic-Image-Stitching-with-Accord-NET
- https://en.wikipedia.org/wiki/Speeded_up_robust_features

--------------

Idea: 'double-check' mode: not only do we correlate (n, n+1), (n+1, n+2) but also (n, n+2). If the latter
has a higher peak than both of the former, we use it as the more "faithful" measure, and calculate (n+1)
by rescaling (n, n+1) and (n+2) with respect to an overall (n, n+2).

Possibilities:
- p(a,c) &gt; max (p(a,b), p(b,c))
- p(a,c) &gt; min (p(a,b), p(b,c))
- p(a,c) &gt; mean(p(a,b), p(b,c))

We'll start with the mean approach.

A   B   C   D
  p1  p2  p3
    p4  p5

---------------

    case class T(translateX: Double, translateY: Double)
    
    val ab = T(0.048448159424886596, 0.2011229386717556)    // peak: 0.5727960173928515
    val bc = T(-0.044944214604982505, 0.22711427149497226)  // peak: 0.39217639163349916
    val ac = T(-1.4088094111775717, 0.48245840171269927)    // peak: 0.7597932510138894
    
    val p1x = ab.translateX + bc.translateX
    val p1y = ab.translateY + bc.translateY
    // import numbers.Implicits._
    val p2x = ab.translateX.linlin(0, p1x, 0, ac.translateX) // BOOM!
    val p2y = ab.translateY.linlin(0, p1y, 0, ac.translateY)
    val p3x = ac.translateX - p2x  // BOOM!
    val p3y = ac.translateY - p2y
    val abT = ab.copy(translateX = p2x, translateY = p2y)
    val bcT = bc.copy(translateX = p3x, translateY = p3y)
    
Either one drops these cases or one clips the values in the interpolation.
Or one leaves ab and uses bcT = ac - ab

In the second case - clipping:

    val p2x = ab.translateX.clip(0, p1x).linlin(0, p1x, 0, ac.translateX)
    val p2y = ab.translateY.clip(0, p1y).linlin(0, p1y, 0, ac.translateY)
    val p3x = ac.translateX - p2x
    val p3y = ac.translateY - p2y
    val abT = ab.copy(translateX = p2x, translateY = p2y)   // T(-1.409,0.227)
    val bcT = bc.copy(translateX = p3x, translateY = p3y)   // T( 0.0  ,0.256)
        
In the third case - leaving ab:

    val bcT = bc.copy(translateX = ac.translateX - ab.translateX,   // T(-1.457,0.281)
                      translateY = ac.translateY - ab.translateY)
    
