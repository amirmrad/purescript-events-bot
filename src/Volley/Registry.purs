module Volley.Registry where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Run (Run, interpret, lift, on, send)
import Run.Except (EXCEPT, note, throw)
import Run.State (STATE, get, modify)
import Type.Proxy (Proxy(Proxy))
import Type.Row (type (+))
import Volley.Types
  ( GuestName
  , Heading
  , Member
  , PlayerId
  , RegistryState
  , Volley
  )

data VolleyRegistryLanguageF a
  = CreateMember PlayerId String a
  | GetMemberById PlayerId (Maybe Member -> a)
  | ModifyMemberById PlayerId (Member -> Member) a
  | NewGame Heading (Volley -> a)
  | AddPlayer (Either GuestName PlayerId) a
  | RemovePlayer (Either (Array Int) PlayerId) a

-- | SetGame Volley a
-- | GetGame (Volley -> a)
-- | PublishGame (String -> a)

derive instance Functor VolleyRegistryLanguageF

_registryCapability :: Proxy "registry"
_registryCapability = Proxy

type REGISTRY r = (registry :: VolleyRegistryLanguageF | r)
type VolleyM r a = Run (REGISTRY + STATE RegistryState + r) a

createMember :: forall r. PlayerId -> String -> VolleyM r Unit
createMember pid name = lift _registryCapability $ CreateMember pid name unit

newGame :: forall r. Heading -> VolleyM r Volley
newGame h = lift _registryCapability $ NewGame h identity

addPlayer :: forall r. (Either GuestName PlayerId) -> VolleyM r Unit
addPlayer p = lift _registryCapability $ AddPlayer p unit

removePlayer :: forall r. (Either (Array Int) PlayerId) -> VolleyM r Unit
removePlayer p = lift _registryCapability $ RemovePlayer p unit

getMemberById :: forall r. PlayerId -> VolleyM r (Maybe Member)
getMemberById pid = lift _registryCapability $ GetMemberById pid identity

modifyMemberById :: forall r. PlayerId -> (Member -> Member) -> VolleyM r Unit
modifyMemberById id f = lift _registryCapability $ ModifyMemberById id f unit

-- getGame :: forall r. VolleyM r Volley
-- getGame = lift _registryCapability $ GetGame identity
-- 
-- setGame :: forall r. Volley -> VolleyM r Unit
-- setGame v = lift _registryCapability $ SetGame v unit

-- publishGame :: forall r. VolleyM r String
-- publishGame = lift _registryCapability $ PublishGame identity

registryToStateAlg
  :: forall r
   . VolleyRegistryLanguageF ~> Run (STATE RegistryState + EXCEPT String + r)
registryToStateAlg =
  ( case _ of
      CreateMember pid name next -> do
        modify \s -> s
          { members = s.members <>
              [ { sid: pid, latestName: name, history: [] } ]
          }
        pure next
      NewGame h next -> do
        let game' = { heading: h, players: [] }
        modify \s -> s { game = Just game' }
        pure $ next game'
      AddPlayer p next -> case p of
        Left _ -> addGuest *> pure next
        Right p' -> addMember p' *> pure next
        where
        addMember :: PlayerId -> _
        addMember p' = do
          s <- get
          maybe (throw "no game")
            ( \game ->
                if Array.elem (Right p') game.players then pure unit
                else do
                  modify \s -> s
                    { game = s.game <#> \g -> g
                        { players = g.players <> [ Right p' ] }
                    , members =
                        map
                          ( \m ->
                              if m.sid == p' then m
                                { history = Array.cons 1 m.history }
                              else m
                          )
                          s.members
                    }
                  pure unit
            )
            s.game
          pure next
        addGuest =
          modify \s -> s
            { game = s.game <#> \g -> g { players = g.players <> [ p ] } }
      RemovePlayer p next -> either removePlayersByIndex removePlayerById p *>
        pure next
        where
        removePlayersByIndex idcs =
          modify \s ->
            s
              { game = s.game <#> \g ->
                  g
                    { players = map snd
                        $ Array.filter
                            (\(Tuple idx _) -> not $ Array.elem idx idcs)
                        $ Array.zip (Array.range 1 (Array.length g.players))
                            g.players
                    }
              }
        removePlayerById id =
          modify \s -> s
            { game = s.game <#> \g -> g
                -- must also update history
                { players = Array.filter ((/=) (Right id)) g.players }
            }
      GetMemberById pid next -> get <#>
        (_.members >>> Array.find ((==) pid <<< _.sid) >>> next)
      ModifyMemberById pid f next -> do
        modify \s -> s
          { members = map (\m -> if m.sid == pid then f m else m) s.members }
        pure next
  -- GetGame next -> get <#> _.game >>= maybe (throw "no game") (pure <<< next)
  -- SetGame v next -> modify (\s -> s { game = Just v }) *> pure next
  -- PublishGame next -> do
  --   ({ game, members } :: RegistryState) <- get
  --   g <- maybe (throw "no game") pure game
  --   gStr <- formatVolley members g
  --   pure $ next gStr
  )

runRegistry
  :: forall r
   . Run (EXCEPT String + REGISTRY + STATE RegistryState + r) ~> Run
       (STATE RegistryState + EXCEPT String + r)
runRegistry = interpret (on _registryCapability registryToStateAlg send)
