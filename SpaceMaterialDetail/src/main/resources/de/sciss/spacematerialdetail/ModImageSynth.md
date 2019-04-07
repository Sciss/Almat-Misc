# Image Synthesizer

Transforms a grayscale image input into a sound file.
This is done by interpreting the x-axis as passing time,
and the y-raxis as frequency axis.

## Parameters

_In:_ Input image file. Should be JPG or PNG grayscale.
  Click on the '...' button to show a file chooser.
  
_Out:_ Path of the target sound file. 
  Click on the '...' button to show a file chooser.
  __Warning:__ will 
  be overwritten when rendering. The file will be 
  monophonic AIFF 24-bit.
  
_Sample Rate:_ Nominal sampling rate of the output file.

_Color:_ Whether to interpret black or white pixels as loudest.
  If the background of your image is white, this should be
  "Black is silent | White is loud" and vice versa.
  
_Duration:_ Target sound file duration in seconds. Image width
  will be scaled accordingly.
  
_Max.freq:_ Frequency in Hertz corresponding to the top row
  of the image
  
_Min.freq:_ Frequency in Hertz corresponding to the bottom row
  of the image.
  
_Max level:_ Oscillator amplitude for the loudest pixels.

_Min level:_ Oscillator amplitude for the most silent pixels.
  The lower this value, the stronger the contrast between black
  and white pixels.
  
_Max voices:_ The number of oscillators to use. The oscillators
  are tuned logarithmically from minimum to maximum frequency.
  If voices is low, you can hear an inharmonic but sinusoidal
  timbre; if the voices are high, you get more of a noise texture.
  Should be no higher than the image height. If the voices are
  less than the image height, not all image rows are scanned by
  oscillators.
  
_Fade in/out:_ An initial fade-in and final fade-out of the 
  synthesised sound, given in seconds.

Once all parameters are set, press 'Render' to start. If an 
exception is shown in the log window, most likely 'In' does not
denote a valid input image or 'Out' does not point to a writable
output file location.
