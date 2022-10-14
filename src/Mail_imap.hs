{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

import           Network.HaskellNet.IMAP.SSL
import           Network.HaskellNet.SMTP.SSL as SMTP

import           Network.HaskellNet.Auth (AuthType(LOGIN), UserName, Password)

import qualified Data.ByteString.Char8 as BC
import qualified Network.Mail.Mime
import qualified Data.Text.Internal.Lazy as B
import qualified Data.Text.Internal as BI


username = "sender@gmail.com"

username1 :: Network.Mail.Mime.Address
username1 = "sender@gmail.com"

password :: Network.HaskellNet.Auth.Password
password = "XXXXXX"

recipient :: Network.Mail.Mime.Address
recipient = "sankaraj06@gmail.com"

imapTest :: IO ()
imapTest = do
    c <- connectIMAPSSLWithSettings "imap.gmail.com" cfg
    login c username password
    mboxes <- list c
    mapM_ print mboxes
    select c "INBOX"
    msgs <- search c [ALLs]
    let firstMsg = head msgs
    msgContent <- fetch c firstMsg
    BC.putStrLn msgContent
    logout c
  where cfg = defaultSettingsIMAPSSL { sslMaxLineLength = 100000 }

smtpTest :: BI.Text ->  B.Text -> IO ()
smtpTest subject body = doSMTPSTARTTLS "smtp.gmail.com" $ \c -> do
    authSucceed <- SMTP.authenticate LOGIN username password c
    if authSucceed
      then sendPlainTextMail recipient username1 subject body c
      else print "Authentication error."
--   where subject = "Test message"
--         body    = "This is a test message"

subject = "Test message"
body    = "This is a test message"

main :: IO ()
main = smtpTest subject body >> imapTest >> return ()

