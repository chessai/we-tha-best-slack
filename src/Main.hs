{-# language
    ImportQualifiedPost
  , LambdaCase
  , OverloadedStrings
#-}

module Main where

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import System.Random (randomRIO)
import Web.Slack
import Web.Slack.Message (sendMessage)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap

main :: IO ()
main = do
  slackConfig <- getSlackConfig
  runBot slackConfig khaledBot ()

getSlackConfig :: IO SlackConfig
getSlackConfig = SlackConfig
  <$> (fromMaybe (fail "SLACK_API_TOKEN not set") <$> lookupEnv "SLACK_API_TOKEN")

khaledBot :: Event -> Slack s ()
khaledBot = \case
  -- a message was sent.
  Message cid _ msg _ _ _ -> do
    when ("another one" `T.isInfixOf` (T.toLower msg)) $ do
      sendMessage cid =<< khaledQuote
    pure ()
  _ -> pure ()

khaledQuote :: MonadIO m => m Text
khaledQuote = liftIO $ do
  quoteIndex <- randomRIO (0, IntMap.size khaledQuotes - 1)
  pure (fromMaybe (error "Khaled quote lookup failed") (IntMap.lookup quoteIndex khaledQuotes))

khaledQuotes :: IntMap Text
khaledQuotes = IntMap.fromList $ zip
  [0..]
  [ "They kick you when you're down, but they wanna kick it when you're up."
  , "You can never run out of keys."
  , "Don't ever play yourself."
  , "You gotta water your plants. No one can water them for you."
  , "Stay focused and secure your bag, because they want you to fail and they don't want us to win."
  , "I'm all about peace. I'm all about unity. I'm all about love."
  , "We gonna win more. We gonna live more. We the best."
  , "I'm gonna go hard no matter what because I gotta feed my family and I gotta feed myself."
  , "The key is that I'm the king."
  , "The key is, to be honest. Be honest, but don't play yourself."
  , "Fresh sneakers are important on a man. It's like a new pair of boxers or a new pair of socks."
  , "I've been a mogul and executive since the beginning of my career. People are just born with that skill."
  , "It's going to work out, stay positive."
  , "Stay positive but stay focused. Sometimes things can distract you and you don't want to be distracted on the journey to that mountain top."
  , "'They' are the people that don't believe in you, that says that you won't succeed. We stay away from 'They'."
  , "I know that I've been put on this Earth to make people happy, to inspire people, and to uplift people. That's a beautiful thing."
  , "Keep all jealous people away from you."
  , "I tell myself every day I love my Jacuzzi, I love my marble floors, I love my high ceilings."
  , "Watch your back, but more importantly when you get out the shower, dry your back. It's a cold world out there."
  , "Hate is a waste of emotion, tell em to jump in the ocean."
  , "That's when you know you're the greatest: when you're the greatest, and people still put odds against you."
  , "You have to work hard for more success."
  , "At the end of the day, I understand that life has roadblocks, and life is like school - you'll be tested; we gotta pass it."
  , "There will be roadblocks but we will overcome it."
  , "Love is the most powerful thing in the world, and you know, what love brings is joy."
  , "Have good relationships."
  , "I put cocoa butter all over my face and my iconic belly and my arms and legs. Why live rough? Live smooth."
  , "Life is amazing, always give thanks no matter what."
  , "When you stop making excuses and you work hard and go hard you will be very successful."
  , "The other day the grass was brown, now its green cuz I ain’t give up. Never surrender."
  , "Keep people around you that’s going to uplift you."
  , "I stayed focused, and I never surrendered, and now I’ve been blessed. now I take care of my mother, my father, and my entire whole family."
  , "They never said winning was easy."
  , "We go hard. In everything we do, we’re going to accomplish our victory and our goal. If it takes a day, a year, or 20 years, we’re going to win. I haven’t taken a loss because everything I’ve done has been a working process to win."
  , "To succeed, you must believe. When you believe, you will succeed."
  , "They don’t want you to win. They don’t want you to have the No. 1 record in the country. They don’t want you to get healthy. They don’t want you to exercise. And they don’t want you to have that view."
  , "More wins, more blessings."
  , "We have to get money. We have no choice. It cost money to eat."
  , "In life, everyone has a choice. The key is: make the right choice."
  , "They will try to close the door on you, just open it."
  , "Walk with me through the pathway to more success."
  , "Key to more success is a clean heart."
  , "You do know it costs money to put a t-shirt on your back? You do know it cost money to have a house? You do know it cost money to eat? Get money, don’t let these people fool you."
  , "Give thanks at all times, we have life."
  , "When you stop making excuses and you work hard and go hard, you will be very successful."
  , "The key is: never fold."
  , "We all want to win more, but it’s all about being blessed and embracing your blessings. We have life."
  , "Always have faith. Always have hope."
  , "I don't think I would run for president."
  , "Nicki Minaj, will you marry me?"
  , "It's safe to say headphones is a good business."
  , "Nicki Minaj, I'm at MTV. I'm going to be honest with you: I love you. I like you. I want you; I want you to be mine. Only reason I'm not telling you this face to face is because I understand that you're busy."
  , "Basically what I'm trying to tell you is that it's almost impossible to drive a jet ski at night time unless you're in a city with lights lit up so you can navigate. Besides being pitch black, that water turn black at night. Listen, I don't recommend it."
  , "No one ever told me I should eat egg whites or drink a gallon of water each day."
  , "Another one."
  , "Another one, no. Another two, drop two singles at a time."
  , "Congratulations, you played yourself."
  , "I can deal with everything. I got the answer for anything. This DJ Khaled."
  , "My fans expect me to be greater and keep being great."
  , "The key is to make it."
  , "The key to more success is cocoa butter."
  , "They don’t want you to jet ski."
  , "Those that weather the storm are the great ones."
  , "You smart! You loyal! You're a genius!"
  , "Them doors that was always closed, I ripped the doors off, took the hinges off. And when I took the hinges off, I put the hinges on the f**kboys’ hands."
  , "Winning, to me, is easy. Winning more is the challenge."
  , "I love having an unlimited supply of cocoa butter."
  , "People will try to bring you down, but you gotta go up."
  , "Sometimes, you might meet somebody that you love that’s turning into a ‘they.’ My key is invite them to Miami and take them to the ocean and let them jump off the boat in the ocean, on the sand bar, and cleanse off and pray and then go take a shower, and hopefully the ‘they’ is out of you."
  , "Basically, I’m one of the greatest producers ever. And I’m also one of the greatest DJs ever. And I’m also one of the best executives ever."
  , "https://www.youtube.com/watch?v=FSswEzWJdWo"
  ]
