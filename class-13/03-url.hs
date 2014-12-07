{-# LANGUAGE TupleSections #-}

import Parser
import SimpleParsers
import Control.Applicative hiding (many, optional)
import Control.Monad

{-
   Определите тип данных, представляющий адрес URL следующего вида:

     <схема>://<логин>:<пароль>@<хост>:<порт>/<URL‐путь>?<параметры>#<якорь>

   Реализуйте соответствующий парсер, считая, что обязательными компонентами
   URL являются только схема с символами "://" и имя хоста, все остальные
   компоненты могут отсутствовать.
-}


data Scheme = FTP | HTTP | HTTPS | Unk String
              deriving Show
type Path = String
type Login = String
type Password = String
type Host = String
type Port = String
type Params = String
type Anchor = String
data URL = URL Scheme (Login, Password) (Host, Port) Path Params Anchor
           deriving Show

scheme = (string "https" >> return HTTPS) <|>
         (string "http" >> return HTTP) <|>
         (string "ftp" >> return FTP) <|>
         Unk `liftM` lowers

loginAndPassword :: Parser (Login, Password)
loginAndPassword = (,) <$> login <*> password
  where
  login = many (sat (/=':'))
  password = char ':' >> many (sat (/='@'))

hostAndPort :: Parser (Host, Port)
hostAndPort = (,) <$> host <*> optional "" port
  where
    host = char '@' >> many (sat (\x -> x/=':' && x/='/'))
    port = char ':' >> many (sat (\x -> x/='/' && x/='?'))

path :: Parser Path
path = char '/' >> many (sat (/='?'))

params :: Parser Params
params = char '?' >> many (sat (/='#'))

anchor :: Parser Anchor
anchor = char '#' >> many (sat (const True))


url = URL <$>
      scheme <*>
      (string "://" >> loginAndPassword) <*>
      hostAndPort <*>
      optional "" path <*>
      optional "" params <*>
      optional "" anchor

testUrl1 = parse url "https://root:admin@www.host.com:80?params#anchor"
testUrl2 = parse url "ftp://root:admin@www.host.com/'sfdasds/sadf'?params"
