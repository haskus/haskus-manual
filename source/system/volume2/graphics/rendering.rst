Graphics Rendering
==================


\subsubsection{FrameBuffer Displaying}

\todo{setControllerFB}

\subsubsection{Buffer Mapping}

\todomize{
   \item Mapping buffers in user-space
   \item Raw drawing
   \item Example rectangle
   \item Dirty fb
}

\section{Configuring The Display}

\todomize{
   \item Mode settings
   \item automatic configuration for the connected devices in preferred modes,
   no cloning
}

Setting the display mode
~~~~~~~~~~~~~~~~~~~~~~~~

Listing~\ref{lst:display_modes} shows a code that tries every available mode of
a connector in sequence: each mode stays enabled for 4 seconds. To be allowed to
change the display mode, we need to create an adapted framebuffer (i.e., a
source of pixel data). You can safely ignore that for now, we will explain it in
details in Section~\ref{sec:framebuffer}.

\pyglistinputfig{codes/src/DisplayModes.hs}{
   label={lst:display_modes},
   caption={Try all available modes}}

\todomize{
   \item mode not supported by display device: garbage, black screen, error
   screen
}


\section{Drawing On The Screen}

\todomize{
   \item Generic framebuffer
   \item full drawing example
}

\todo{Example DPMS (power-off)}


\section{Multiple-Buffering And V-Blank Synchronization}
\label{sec:multi-buffering}

Computer screens usually have a fixed refresh rate. For instance in
Listing~\ref{lst:display_connectors_result} the first mode of the connected
display device has a (vertical) refresh rate of 60Hz. That is, 60 times per
second, the display device:

\begin{enumerate}
   \item copies the contents of the framebuffer on the screen line-by-line:
   \emph{scan-out} period
   \item waits for some time until the next scan-out: \emph{v-blank} period
\end{enumerate}

\subsubsection{Single Frame Buffer}

Suppose we use a single frame buffer that is displayed on the screen. If we
modify its content during the scan-out period, some artifacts may appear on the
screen. For instance if we repeatedly clear the screen, then draw a filled
rectangle and finally draw a circle on top of it, the display may either show
the cleared screen, the rectangle alone or the rectangle with the circle. The
rectangle and the circle seem to flash: this is called a \emph{flicker} effect.

\todo{time to render a frame vs refresh period vs v-blank period}


\todomize{
   \item Explanation scan-out (flikering?)
   \item Explanation multi-buffering
   \item Code framebuffer swap ("page flip")
   \item Explanation v-blank (tearing?)
   \item Code synchro v-blank (event v-blank)
   \item Note "async page flip" flag and "page flip complete" event
   \item Adaptive v-sync
   \item Dithering (frame rate control, TN panels 6-bits per RGB)
}

\section{Advanced Topics}

\subsection{Atomic Configuration}

Some drivers support an atomic configuration operation. Instead of setting a
property at a time, several properties are set at once in a transaction: if a
proprety fails to be set, the whole transaction is reverted. It ensures that the
display configuration is never left in a transition state between two
configurations.\footnote{At the time of writing, ``haskus-system`` doesn't
support this feature.}

\subsection{Gamma Table}

\todo{theory, tests avec qemu non concluants}

Gamma correction consists in altering the way colors are displayed on a monitor.
For each possible value of each color channel (red, green, blue),we can define a
gamma factor. Usually there are 256 possible values per channel, but the
additional \texttt{controllerGammaTableSize} field gives the actual number of
values. Each factor is an unsigned 16-bit word.

\todo{Word16 or 8.8 fixed float?}

Listing~\ref{lst:display_gamma} shows how to retrieve and show the gamma look-up
table of a controller with \texttt{getControllerGamma}. Similarly you can set
another gamma table with \texttt{setControllerGamma}.

\pyglistinputfig{codes/src/DisplayGamma.hs}{
   label={lst:display_gamma},
   caption={Show controller gamma table}}

\subsection{Sub-Pixel Rendering}
\label{sec:subpixel}

\todomize{
   \item Controller sub-pixel
   \item Used for fonts
   \item Vertical vs horizontal
   \item rotation property!
}



