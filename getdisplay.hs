import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Foldable (forM_) 
import Data.IORef
import Foreign
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk 
import Graphics.X11.Xlib as X11 
-- 
import Graphics.Rendering.EGL.Raw
import Graphics.Rendering.EGL.Raw.Types 

main :: IO ()
main = do 
  initGUI
  megldpy <- runMaybeT $ do 
    gdkdpy <- MaybeT displayGetDefault
    str <- liftIO $ displayGetName gdkdpy 
    x11dpy <- liftIO $ X11.openDisplay str 
    let X11.Display x11dpyptr = x11dpy 
    eglnativedpytyp <- liftIO $ mkEGLNativeDisplayType (castPtr x11dpyptr)
    egldpy <- liftIO $ eglGetDisplay eglnativedpytyp
    return egldpy 
  forM_ megldpy $ \egldpy -> do 
    widthptr <- malloc 
    heightptr <- malloc
    eglInitialize egldpy widthptr heightptr
    width <- peek widthptr 
    height <- peek heightptr 
    print (width,height) 
    gtkroutine egldpy  
    eglTerminate egldpy
    free widthptr 
    free heightptr  

gtkroutine egldpy = do
  window <- windowNew 
  vbox <- vBoxNew False 0 
  canvas <- drawingAreaNew 
  set canvas [ widgetWidthRequest := 400 
             , widgetHeightRequest := 400 ]   
  boxPackStart vbox canvas PackNatural 0   
  containerAdd window vbox 
  canvas `on` exposeEvent $ tryEvent $ do 
    win <- liftIO $ widgetGetDrawWindow canvas 
    liftIO . renderWithDrawable win $ do 
      setSourceRGBA 0 0 0 1 
      setLineWidth 1.0
      moveTo 100 100 
      lineTo 200 200 
      stroke
  window `on` deleteEvent $ tryEvent $ do 
    liftIO $ mainQuit 
  widgetShowAll window
  mainGUI 


