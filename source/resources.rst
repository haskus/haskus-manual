==============================================================================
Resources
==============================================================================

------------------------------------------------------------------------------
Graphics
------------------------------------------------------------------------------

http://www.scratchapixel.com/

  A lot of resources ("lessons") about Computer Graphics. Especially `describes
  <http://www.scratchapixel.com/lessons/3d-basic-rendering/rasterization-practical-implementation/overview-rasterization-algorithm>`_
  the whole 3D pipeline: triangle rasterization (projection, coverage test,
  depth management, etc.).

`How OpenGL works: software rendering in 500 lines of code <https://github.com/ssloy/tinyrenderer/wiki>`_

   Another set of lessons to build a rendered.


http://blog.johnnovak.net/2016/09/21/what-every-coder-should-know-about-gamma/

  Explanation of gamma. Takeover: most operations (gradient, blending, etc.)
  must be done in linear space and in floating-point to be correct. Using sRGB
  space as if it is linear is wrong.

http://poynton.ca/PDFs/YUV_and_luminance_harmful.pdf

   "YUV and Luminance considered harmful". Takeover: "luminance" (Y') in
   computer graphics is not the same as "lumninance" (Y) in color science (i.e.
   CIE luminance). We should avoid the terms YUV and "luminance" and use the
   correct "Y'C_BC_R" and "luma" terms instead.

------------------------------------------------------------------------------
Input
------------------------------------------------------------------------------

http://who-t.blogspot.com/2018/07/why-its-not-good-idea-to-handle-evdev.html

   Describe how difficult it is to handle input events with the experience of
   libinput.
