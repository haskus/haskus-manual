===================
Linux display model
===================

Linux provides an API named *kernel mode setting* (KMS) that allows us to
control video displays. The idea is to build a pipeline that describes where the
video displays get their pixel colors from.

The left side of the following picture describes the relations between pipeline entities and the right side shows a more visual representation of how an image is formed on a video display surface using the same entities.


.. image:: /_static/images/system/graphics_linux_model.svg
   :class: img_center


Modifying the graphics pipeline
-------------------------------

To use a video display, our task is to build such valid pipeline. There are so
many possible hardware configurations (different graphics chipsets, different
video displays, etc.) that the KMS interface is very generic. It lets us build a
pipeline quite liberally and then we can test it before enabling it for real if
it is valid.

Controller and Plane entities of the graphics pipeline are fixed for each
graphics chipset. However Connectors are *not* fixed because some technologies
(such as `DisplayPort Multi-Stream Transport
<https://en.wikipedia.org/wiki/DisplayPort#Multi-Stream_Transport_(MST)>`_)
allows the use of connectors hubs which dynamically add additional Connector
entities.



Listing connectors
------------------

The following code lists the graphic cards, their connectors and if a video
display is connected to the connector: its modes and its properties.

.. code:: haskell

   {-# LANGUAGE OverloadedStrings #-}
   {-# LANGUAGE BlockArguments #-}

   import Haskus.System
   import Haskus.System.Linux.Graphics.State
   import Haskus.System.Linux.Graphics.Property
   import Haskus.System.Linux.Graphics.Object
   import Haskus.System.Linux.Graphics.Mode

   main :: IO ()
   main = runSys' do
      sys   <- defaultSystemInit
      term  <- defaultTerminal

      cards <- loadGraphicCards (systemDeviceManager sys)
      
      forM_ cards \card -> do

         state <- readGraphicsState (graphicCardHandle card)
                     |> assertLogShowErrorE "Read graphics state"

         let conns = graphicsConnectors state

         when (null conns) do
            writeStrLn term "No connector found"

         forM_ conns \conn -> do
            writeStrLn term ("Probing " ++ getObjectQualifiedID conn)

            case connectorState conn of
               Disconnected      -> writeStrLn term " -> disconnected"
               ConnectionUnknown -> writeStrLn term " -> unknown connection"
               Connected dev -> do
                  writeStrLn term "Modes"
                  forM_ (connectedDeviceModes dev) \mode ->
                     writeStrLn term (showMode mode)

                  writeStrLn term "Properties"
                  forM_ (connectedDeviceProperties dev) \prop ->
                     writeStrLn term ("    " ++ showProperty prop)

When run into QEmu with Linux 5.1.15 it returns:

.. code:: text

   Probing Connector 33
   Modes
   1024x768 60MHz -HSync -VSync
           h: width 1024 start 1048 end 1184 total 1344 skew    0
           v: width  768 start  771 end  777 total  806 scan    0
   1920x1080 60MHz -HSync -VSync
           h: width 1920 start 2008 end 2052 total 2200 skew    0
           v: width 1080 start 1084 end 1089 total 1125 scan    0
   1600x1200 60MHz +HSync +VSync
           h: width 1600 start 1664 end 1856 total 2160 skew    0
           v: width 1200 start 1201 end 1204 total 1250 scan    0
   [...]
   Properties
       var DPMS = On :: Enum [On,Standby,Suspend,Off]
       var link-status = Good :: Enum [Good,Bad]
       val non-desktop = False :: Bool
       var CRTC_ID = 0 :: Object
