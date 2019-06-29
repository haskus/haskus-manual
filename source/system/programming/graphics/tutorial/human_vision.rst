========================
Human vision and screens
========================

Screen technology is based on human vision in several ways:

Red, green, blue
----------------

The human eye uses 3 kinds of sensors (in daylight) called `cones
<https://en.wikipedia.org/wiki/Cone_cell>`_, each covering some part of the
`visible spectrum <https://en.wikipedia.org/wiki/Visible_spectrum>`_. Their
behavior is quite complicated because their response ranges overlap, the count
of each sensor differs, the light perception is non-linear, etc. For the human
eye, different `spectral power distributions
<https://en.wikipedia.org/wiki/Spectral_power_distribution>`_ can lead to the
same perceived color (`metamerism
<https://en.wikipedia.org/wiki/Metamerism_(color)>`_).

Long story short, a screen doesn't have to be able to produce every frequency of
the visible spectrum to reproduce colors: it just has to produce 3 of them to
trigger each kind of cone at a different level. Typically a screen produces
color by combining different amount of red, green and blue lights which roughly
correspond to each cone response peak.

Usually display devices can only produce a subset of the visible color space (or
`gamut <https://en.wikipedia.org/wiki/Gamut>`_).

Spatial integration
-------------------

If several lights of different colors are emitted very closely to each other,
our eyes can't distinguish them and we don't perceive the gap between them.
Instead we only perceive another color which is composed of the emitted ones.

Screens display an array of lights called pixels (say 1920x1080 pixels) which
are very close one to the other so that we perceive a continuous picture without
distinguishing the gaps between each pixel individually (at normal viewing
distance of the screen).

Screens display pixel themselves as a combination of several disctint lights
(red, green blue) which are so close that we perceive as a single color.


Temporal integration
--------------------

Similarly to the spatial integration, our eyes cannot distinguish colors that
change too fast over time. Instead we perceive an integration of the light over
time.

Screens use this to simulate continuous motion: if different images are
displayed very quickly, our eyes don't distinguish each picture individually.
Some screens even emit a single line of pixels at a time or a single pixel
at a time, or alternate between even and odd lines for each frame.


Summary
-------

Rendering an image on a screen consists in indicating the color of each pixel.
Pixels are samples of the image we would like to render on the screen if we had
an infinite resolution, not "small squares".

To simulate a continuous movement on the screen, we have have to render many
different images per seconds.

I recommend watching this video: `How a TV works in slow motion <https://www.youtube.com/watch?v=3BJU2drrtCM>`_
