# Image Synthesizer

Transforms a grayscale image input into a sound file.
This is done by interpreting the x-axis as passing time,
and the y-axis as frequency axis.

Parameters:

- __In__: Input image file. Should be JPG or PNG grayscale.
  Click on the '...' button to show a file chooser.
- __Out__: Path of the target sound file. 
  Click on the '...' button to show a file chooser.
  __Warning:__ will 
  be overwritten when rendering. The file will be 
  monophonic AIFF 24-bit.
- __Sample Rate__: Nominal sampling rate of the output file.
- __Color__: Whether to interpret black or white pixels as loudest.
  If the background of your image is white, this should be
  "Black is silent | White is loud" and vice versa.
- __Duration__: Target sound file duration in seconds. Image width
  will be scaled accordingly.
- __Max.freq__: Frequency in Hertz corresponding to the top row
  of the image
- __Min.freq__: Frequency in Hertz corresponding to the bottom row
  of the image.
- __Max level__: Oscillator amplitude for the loudest pixels.
- __Min level__: Oscillator amplitude for the most silent pixels.
  The lower this value, the stronger the contrast between black
  and white pixels.
- __Max voices__: The number of oscillators to use. The oscillators
  are tuned logarithmically from minimum to maximum frequency.
  If voices is low, you can hear an inharmonic but sinusoidal
  timbre; if the voices are high, you get more of a noise texture.
  Should be no higher than the image height. If the voices are
  less than the image height, not all image rows are scanned by
  oscillators.
- __Fade in/out__: An initial fade-in and final fade-out of the 
  synthesised sound, given in seconds.

Once all parameters are set, press 'Render' to start. If an 
exception is shown in the log window, most likely 'In' does not
denote a valid input image or 'Out' does not point to a writable
output file location.
