===================
Linux display model
===================

Linux provides an API named *kernel mode setting* (KMS) that can be used to
query and to configure graphics chipsets.

It allows the software to query the connected video displays and their available
modes (resolution, refresh rate, etc.) and to indicate which mode and planes to
use and where to fetch the pixel colors from.

There are so many possible configurations (different graphics chipsets,
different video displays, etc.) that the KMS interface is very generic. It lets
you build a configuration quite liberally using objects (connectors,
controllers, planes, etc.) and their properties. Then you can test the built
configuration before enabling it for real.

For instance some hardware may have two output connectors but only supports full
resolution if a single video display is used (otherwise the resolution must be
halved) for bandwidth reason. Currently there is no way to query the hardware
and discover this, but we can test the different configurations and select the
valid one we prefer (or let the user choose).

Listing connectors
------------------

.. note::

   Some devices support connector hubs. Hence the list of connectors may change
   dynamically.

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
