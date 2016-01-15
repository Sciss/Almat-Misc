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
