import XMonad

-- n.b. the following code is inlined & badly ported from XMonad.Config.Gnome
-- from the XMonad Contrib packages:

-- original code by:
--      Copyright    : (c) Spencer Janssen <spencerjanssen@gmail.com>
--      License      : BSD
-- ported from GNOME to MATE by:
--      Copyright    : (c) rfc <reuben.fletchercostin@gmail.com>
--      License      : BSD

import XMonad.Config.Desktop
import XMonad.Util.Run (safeSpawn)

import qualified Data.Map as M

import System.Environment (getEnvironment)

import XMonad.Actions.PhysicalScreens


-- $usage
-- To use this module, start with the following @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Config.Mate
-- >
-- > main = xmonad mateConfig
--
-- For examples of how to further customize @mateConfig@ see "XMonad.Config.Desktop".

mateConfig = desktopConfig
    { terminal = "terminator"
    , keys     = mateKeys <+> keys desktopConfig
    , startupHook = mateRegister >> startupHook desktopConfig }

mateKeys (XConfig {modMask = modm}) = M.fromList $
    [ ((modm, xK_p), mateRun)
--    , ((modm .|. shiftMask, xK_q), spawn "mate-session-save --kill")
--    , ((modMask, xK_a), onPrevNeighbour W.view)
--    , ((modMask, xK_o), onNextNeighbour W.view)
--    , ((modMask .|. shiftMask, xK_a), onPrevNeighbour W.shift)
--    , ((modMask .|. shiftMask, xK_o), onNextNeighbour W.shift)
 ] ++
     [((modm .|. mask, key), f sc)
     | (key, sc) <- zip [xK_q, xK_w, xK_e] [0..]
     , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

-- | Launch the "Run Application" dialog.  mate-panel must be running for this
-- to work.
mateRun :: X ()
mateRun = withDisplay $ \dpy -> do
    rw <- asks theRoot
    mate_panel <- getAtom "_MATE_PANEL_ACTION"
    panel_run   <- getAtom "_MATE_PANEL_ACTION_RUN_DIALOG"

    io $ allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw mate_panel 32 panel_run 0
        sendEvent dpy rw False structureNotifyMask e
        sync dpy False

-- | Register xmonad with mate. 'dbus-send' must be in the $PATH with which
-- xmonad is started.
--
-- This action reduces a delay on startup only only if you have configured
-- mate-session>=2.26: to start xmonad with a command as such:
--
-- > mateconftool-2 -s /desktop/mate/session/required_components/windowmanager xmonad --type string
mateRegister :: MonadIO m => m ()
mateRegister = io $ do
    x <- lookup "DESKTOP_AUTOSTART_ID" `fmap` getEnvironment
    whenJust x $ \sessionId -> safeSpawn "dbus-send"
            ["--session"
            ,"--print-reply=string"
            ,"--dest=org.mate.SessionManager"
            ,"/org/mate/SessionManager"
            ,"org.mate.SessionManager.RegisterClient"
            ,"string:xmonad"
            ,"string:"++sessionId]

-- end of inlined mateConfig


-- here we actually configure xmonad
main = xmonad mateConfig 

