# xCoAx2020 Postcard

[![Build Status](https://travis-ci.org/Sciss/xcoax2020-postcard.svg?branch=master)](https://travis-ci.org/Sciss/xcoax2020-postcard)

## statement

Software for an art project. (C)opyright 2019 by Hanns Holger Rutz. All rights reserved. This project is released 
under the [GNU Affero General Public License](http://git.iem.at/sciss/xcoax2020-postcard/blob/master/LICENSE) v3+ and comes 
with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`.

## building

Builds with sbt against Scala 2.13.

## running

The steps are:

- `RunGNG`
- `CenterImages`
- `Composition`
- `RotateSetDPI` (requires image magick)
- `MakePDF` (requires Inkscape)
- `JoinPDF` (required pdftk)

Image Magick for converting to 2-color png, setting 600 dpi resolution, rotating anti-clockwise:

    convert outcc0001.png -monochrome -density 600 -rotate -90 outcc0001bw.png

Inkscape convert svg to pdf

    inkscape -A postal_xcoax2020almat_link.pdf postal_xcoax2020almat_link.svg
