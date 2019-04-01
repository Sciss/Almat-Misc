# Phase Difference

Transforms a sequence of images obtained from a moving image (video)
input another sequence of images based on the differential phase correlation
of successive frames. This is a form of motion analysis.

The output size will be square with side length equal to the
next smaller power of two of the minimum of the input image's width and height.
For example, if the input image size is 1920 x 1080 pixels, then
the smaller value is 1080 and then next smaller power of two of 1080 is 1024,
thus the output images will be of size 1024 x 1024 pixels.

The output sequence will have a length `Input Resampling Factor * Output Resampling Factor`
times of the input length.

Parameters:

- __Input Image Sequence__: Input image file "template". The input files should 
  be JPG or PNG grayscale. Click on the '...' button to show a file chooser.
  Select one of the input images and confirm. Then you _have to insert a %d placeholder_
  in the file name. This denotes the position of the frame index.
  For example, if your
  input image sequence consists of the files `input-1.jpg`, `input-2.jpg` through `input-100.jpg`,
  make sure you write `input-%d.jpg` here.
- __Output Image Sequence__: Path of the target image sequence "template.
  Click on the '...' button to show a file chooser.
  As in the input
  sequence, the file name _must have a %d placeholder_ for the frame index. 
  For example, if your
  output image sequence should producethe files `output-1.jpg`, `output-2.jpg` etc.,
  make sure you write `output-%d.jpg` here.
  __Warning:__ will 
  be overwritten when rendering. The file will be 
  grayscale image. If the template name ends in .jpg it will use JPEG, if it
  ends in .png, it will use PNG encoding.
- __Sample Rate__: Nominal sampling rate of the output file.
- __Input First Frame__: Index for the input sequence template at which to begin.
  For example, if the first frame is 12 and the input image sequence is `input-%d.jpg`,
  the program will start reading `input-12.jpg`, followed by `input-13.jpg` etc.
- __Input Last Frame__: Index for the input sequence template at which to end.
  For example, if the last frame is 66 and the input image sequence is `input-%d.jpg`,
  the program will keep reading until `input-66.jpg`.
- __Input Resampling Factor__: A time stretch factor for the input data
  (before applying phase analysis). A value of
  1 means no time stretching, a value of 2 means to stretch the time by factor two
  (slow down to 50% of the speed).
  Resampling requires heavy CPU usage, so a value
  other than 1 will slow down the rendering.
- __Output Resampling Factor__: A time stretch factor for the output data
  (after applying phase analysis). A value of
  1 means no time stretching, a value of 2 means to stretch the time by factor two
  (slow down to 50% of the speed).
  Resampling requires heavy CPU usage, so a value
  other than 1 will slow down the rendering.
- __Noise Amount__: A value between zero and one to determine the amplitude of
  white noise added to the output. Zero means no noise. The more noise is added, the
  larger the files will get, because it will be more complicated to compress them.

Once all parameters are set, press 'Render' to start. If an 
exception is shown in the log window, most likely 'In' does not
denote a valid input image or 'Out' does not point to a writable
output file location.
