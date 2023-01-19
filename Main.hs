module Main (main) where

--import Lib
import Control.Concurrent
import System.Random

type UserName = String
type Messages = String
type MsgCount = Integer

data User = User {
      username :: UserName
   } deriving (Show)

data Message = Message {
      message :: Messages
} deriving (Show)

{- 
data AllMessage = AllMessage {
      from :: User,
      to :: User,
      text :: Messages
} deriving (Show) -}

--allMessage = AllMessage {(from, to, text)}

tom = User {username =      "Tom    "}
jake = User {username =     "Jake   "}
sully = User {username =    "Sully  "}
tony = User {username =     "Tony   "}
mike = User {username =     "Mike   "}
kelly = User {username =    "Kelly  "}
rachael = User {username =  "Rachael"}
alice = User {username =    "Alice  "}
fred = User {username =     "Fred   "}
anthony = User {username =  "Anthony"}

a = Message {message = "Greetings!"}
b = Message {message = "Pub?"}
c = Message {message = "Pub first, then cinema?"}
d = Message {message = "I'm staying in tonight"}
e = Message {message = "Don't Stay in tonight"}
f = Message {message = "Okay"}
g = Message {message = "Sorry, I have things to do right now. Can we talk later?"}
h = Message {message = "Let's get a bite to eat"}
i = Message {message = "I am in a hurry, let me know what is happening"}
j = Message {message = "Today will be fun!"}

mapIntToPerson :: Int -> User
mapIntToPerson n = case r of
      0 -> tom
      1 -> jake
      2 -> sully
      3 -> tony
      4 -> mike
      5 -> kelly
      6 -> rachael
      7 -> alice
      8 -> fred
      9 -> anthony
    where r = mod n 10 

mapIntToMessage :: Int -> Message
mapIntToMessage n = case r of
      0 -> a
      1 -> b
      2 -> c
      3 -> d
      4 -> e
      5 -> f
      6 -> g
      7 -> h
      8 -> i
      9 -> j
    where r = mod n 10

mapIntToRandInterval :: Int -> Int
mapIntToRandInterval n = case r of
      0 -> 500000
      1 -> 1000000
      2 -> 2000000
      3 -> 3000000
      4 -> 4000000
      5 -> 5000000
    where r = mod n 6

randUsr :: IO User
randUsr = do
    n <- randomIO :: IO Int
    let person = mapIntToPerson n
    return person

randMessage :: IO Message
randMessage = do
    n <- randomIO :: IO Int
    let messages = mapIntToMessage n
    return messages

randInterval :: IO Int
randInterval = do
    n <- randomIO :: IO Int
    let interval = mapIntToRandInterval n
    return interval

sndMessage :: User -> MVar () -> MVar (User, User, Message) -> IO ()
sndMessage user io chats = do
    threadId <- myThreadId
    ru <- randUsr
    msg <- randMessage
    interval <- randInterval
    threadDelay interval
    --putStrLn $ show threadId ++ " From: " ++ show user ++ " To: " ++ show(ru) ++ " " ++ show(msg)
    putMVar chats (user, ru, msg)
    allMessage <- takeMVar chats -- ++ allMessage
    putStrLn $ show allMessage
    --allMessage <- x
    sndMessage user io chats

main :: IO ()
main = do
    io <- newMVar ()
    person <- randUsr
    chats <- newEmptyMVar

    threadTom <- newEmptyMVar
    threadJake <- newEmptyMVar
    threadSully <- newEmptyMVar
    threadTony <- newEmptyMVar
    threadMike <- newEmptyMVar
    threadKelly <- newEmptyMVar
    threadRachael <- newEmptyMVar
    threadAlice <- newEmptyMVar
    threadFred <- newEmptyMVar
    threadAnthony <- newEmptyMVar

    threadTom <- forkIO (sndMessage tom io chats)
    threadJake <- forkIO (sndMessage jake io chats)
    threadSully <- forkIO (sndMessage sully io chats)
    threadTony <- forkIO (sndMessage tony io chats)
    threadMike <- forkIO (sndMessage mike io chats)
    threadKelly <- forkIO (sndMessage kelly io chats)
    threadRachael <- forkIO (sndMessage rachael io chats)
    threadAlice <- forkIO (sndMessage alice io chats)
    threadFred <- forkIO (sndMessage fred io chats)
    threadAnthony <- forkIO (sndMessage anthony io chats)

    threadDelay 20000000 --I need this until the messages can be counted

    putStrLn $ "<<<<<<<<--------Last Line-------->>>>>>>>"
    