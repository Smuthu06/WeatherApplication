{-# LANGUAGE OverloadedStrings #-}

import Network.HaskellNet.SMTP
import Network.HaskellNet.Auth ()
import Network.Mail.Mime ( simpleMail', Address )
import System.Exit (die)

main :: IO ()
main = doSMTP "smtp.gmail.com" $ \conn -> do -- (1)
   authSucceed <- authenticate LOGIN "sndermail" "password" conn -- (2)
   if authSucceed
   then do
     let mail = simpleMail'
           ("weathermap.haskell@gmail.com" :: Address)
           ("sankaraj06@gmail.com" :: Address )
           "subject"
           "Hello! This is the mail body!"
     sendMail mail conn -- (3)
   else die "Authentication failed."