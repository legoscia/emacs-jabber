import Test.QuickCheck (Arbitrary, arbitrary, shrink, Property, quickCheck, (==>), choose,
                        oneof, quickCheckWithResult, stdArgs, Args(..), Result(..))
import Test.QuickCheck.Arbitrary (shrinkList, shrinkNothing)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import Test.QuickCheck.Property (printTestCase)
import Test.QuickCheck.Gen (Gen)
import Data.List (nub)
import Data.Char (toLower)
import Control.Monad (liftM, mfilter)

data RosterEvent = IqRoster JID [Group] (Maybe String) |
                   IqRosterDelete JID |
                   Presence JID PresenceType |
                   Noop
                   deriving (Show, Eq)
instance Arbitrary RosterEvent where
  arbitrary = do jid <- arbitrary
                 oneof
                   [ do groups <- arbitrary
                        maybeName <- arbitraryName
                        return $ IqRoster jid (nub groups) maybeName,
                     return $ IqRosterDelete jid,
                     do presenceType <- arbitrary
                        return $ Presence jid presenceType]
    where arbitraryName :: Gen (Maybe String)
          arbitraryName = oneof [return Nothing,
                                 liftM Just arbitrarySensibleString]
  shrink Noop = []
  shrink (IqRoster (JID j) groups name) =
    [Noop] ++
    [IqRoster (JID j) newGroups newName |
     newGroups <- (shrinkList shrink groups),
     newName <- shrink name]
  shrink _ = [Noop]

arbitrarySensibleString =
  do arbitraryString <- arbitrary
     return $ filter (\c -> c >= ' ' && c <= '~') arbitraryString

data JID = JID String deriving (Eq)
instance Arbitrary JID where
  arbitrary = do
    x <- choose ('a', 'e')
    return . JID $ [x] ++ "@example.com"
instance Show JID where
  show (JID s) = s

data Group = Group String deriving (Eq)
instance Arbitrary Group where
  arbitrary = do
    x <- choose ('a', 'e')
    return . Group $ [x]
  shrink (Group (x:[])) = [Group [y] | y <- ['a' .. (pred x)]]
instance Show Group where
  show (Group s) = s

data PresenceType = Unavailable | Online | Chat | Away | XA | DND deriving (Show, Eq)
instance Arbitrary PresenceType where
  arbitrary = oneof $ map return [Unavailable, Online, Chat, Away, XA, DND]

main = do result <- quickCheckWithResult (stdArgs { chatty = False }) prop_rosterEvents
          case result of
           Success {} -> putStrLn "success"
           Failure { output = o } -> putStrLn $ "failure: " ++ o

prop_rosterEvents :: [RosterEvent] -> Property
prop_rosterEvents events =
  printTestCase ("counterexample: " ++ unlines asLisp) $ monadicIO test
  where test = do result <- run testIO
                  assert (result == "t")
        testIO = do mapM putStrLn asLisp
                    putStrLn "check"
                    getLine
        asLisp = map toLisp events

toLisp (IqRoster (JID jid) groups maybeName) =
  "(iq ((type . \"set\"))"++
  "  (query ((xmlns . \"jabber:iq:roster\"))" ++
  "    (item ((jid . \""++jid++"\")" ++
  maybe "" (\name -> " (name . " ++ show name ++ ")") maybeName ++
  ") "++
  concat ["(group () \""++group++"\")" | (Group group) <- groups] ++
  " )))"
toLisp (IqRosterDelete (JID jid)) =
  "(iq ((type . \"set\"))"++
  "  (query ((xmlns . \"jabber:iq:roster\"))" ++
  "    (item ((jid . \""++jid++"\") (subscription . \"remove\")))))"
toLisp (Presence (JID jid) Unavailable) =
  "(presence ((from . \""++jid++"\") (type . \"unavailable\")))"
toLisp (Presence (JID jid) Online) =
  "(presence ((from . \""++jid++"\")))"
toLisp (Presence (JID jid) presenceType) =
  "(presence ((from . \""++jid++"\")) "++
  "  (show () \""++(map toLower (show presenceType))++"\"))"
toLisp Noop = ""
