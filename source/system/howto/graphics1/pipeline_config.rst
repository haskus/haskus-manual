.. _graphics_pipeline_config:

==============================================================================
Configuring the pipeline
==============================================================================

At this stage we already know enough things to :ref:`detect a connected video
display <graphics_listing_displays>` and to :ref:`allocate generic buffers and
frames <graphics_generic_buffers>`. We just need to learn how to connect
entities to build :ref:`a pipeline <graphics_pipeline>` and finally display
something on the screen.

The whole source code can be found
`here
<https://github.com/haskus/haskus-system/blob/master/haskus-system-examples/src/tutorial/TutFirstPipeline.hs>`_.

We first need to find a primary plane and check the pixel formats it supports.
Then we need a Controller that can work with the plane (obtained with
``planePossibleControllers``). Finally we need to build the pipeline with code
similar to the following one:

.. code:: haskell

   assertLogShowErrorE "Config" <| withModeBlob card mode \modeBlobID ->
      configureGraphics card Commit EnableVSync EnableFullModeset do
         setConnectorSource conn ctrlID -- connector  <-> controller
         setPlaneTarget plane ctrlID    -- controller <-> plane
         setPlaneSource plane frame     -- plane      <-> frame
         -- sizes and mode
         setPlaneSize plane (frameWidth frame) (frameHeight frame)
         setPlaneSourceSize plane (frameWidth frame) (frameHeight frame)
         setMode ctrlID modeBlobID
         -- enable the controller
         enableController ctrlID True

We allocate a Mode blob on the graphic card with ``withModeBlob``. Then we use
``configureGraphics`` to send a batch of commands to the chipset: the commands
will either all succeed or none of them will be applied (atomicity).

To test that a batch of commands is valid, you can pass ``TestOnly`` instead of
``Commit`` and check for errors.

You can allow or disallow a full mode-setting with ``EnableFullModeset`` and
``DisableFullModeset``. A full mode-setting is costly so avoid it if you can
afford to run in a degraded mode for some time.

You can enable or disable :ref:`vertical synchronization (VSYNC) <graphics_vsync>`.
It is recommended to enable it to avoid tearing and because some drivers seem to
require it.

The example code should display the following pattern on the screen:

.. image:: /_static/images/system/graphics_first_pipeline.png
   :class: img_center
