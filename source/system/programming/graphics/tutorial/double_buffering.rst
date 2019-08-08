.. _graphics_double_buffering:

==============================================================================
Double-bufffering and frame-switching
==============================================================================

In this chapter, we see that directly modyfing the frame that is displayed on
the screen leads to flikering and that double-buffering should be used instead.

Starting from the code of the :ref:`previous chapter
<graphics_pipeline_config>`_, suppose that we modify the contents of the frame
continuously as we do 
`here
<https://github.com/haskus/haskus-system/blob/master/haskus-system-examples/src/tutorial/TutSingleFrame.hs>`_:

.. code:: haskell


   let render frame col = do
        -- fill a frame with a color
        forEachGenericFramePixel frame 0 \_x _y ptr -> do
           poke ptr (col :: Word32)
   
       renderLoop col = do
         render frame col
         renderLoop (col + 10) -- change the color for each frame
   
   sysFork "Render loop" (renderLoop 0)

If we try to execute this example, we see some flikering: sometimes the
displayed frame is not fully repaint and it has two different colors, that's why
we see some vertical line demarcating both colors.

This is a common issue that can be solved either by:

* only doing the rendering during the vblank period
* using two different frames and switching them during the vblank period

The first solution only requires a single buffer but your application has to
render each frame very fast during the vblank period and before the end of the
refresh cycle.

Using double-buffering is easier. The modified code example using
double-buffering is
`here
<https://github.com/haskus/haskus-system/blob/master/haskus-system-examples/src/tutorial/TutFrameSwitch.hs>`_.
The change consists in allocating two frames, rendering in one when the other is
displayed and switching the frames when the rendering is over:

.. code:: haskell

   let switchFrame frame = assertLogShowErrorE "Switch frame" <| do
         configureGraphics card Commit EnableVSync DisableFullModeset do
            setPlaneSource plane frame
   
   frame1 <- createGenericFullScreenFrame card mode pixelFormat 0
   frame2 <- createGenericFullScreenFrame card mode pixelFormat 0
   
   let renderLoop b col = do
         let frame = if b then frame1 else frame2
         render frame col
         switchFrame frame
         renderLoop (not b) (col + 10)
   
   sysFork "Render loop" (renderLoop False 0)
