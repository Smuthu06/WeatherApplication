{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

import           Network.HaskellNet.IMAP.SSL
import Network.HaskellNet.SMTP.SSL as SMTP
    ( authenticate, sendPlainTextMail, doSMTPSTARTTLS )
import           Network.HaskellNet.Auth (Password)

import qualified Data.ByteString.Char8 as B
import Network.Mail.Mime (Address)
--import qualified Data.Text.Internal.Lazy.Text as LT


username1 :: [Char]
username1 = "weathermap.haskell@gmail.com"

username :: Address
username = "weathermap.haskell@gmail.com"

password :: Network.HaskellNet.Auth.Password
password = "Emurgo@123"

recipient :: Address
recipient = "sankaraj06@gmail.com"

smtpTest :: IO ()
smtpTest = doSMTPSTARTTLS "smtp.gmail.com" $ \c -> do
    authSucceed <- SMTP.authenticate LOGIN username1 password c
    if authSucceed
      then sendPlainTextMail recipient username subject body c
      else print ("Authentication error." :: String)
  where subject =  "Test message"
        body    =  "This is a test message"

main :: IO ()
main = do
  smtpTest