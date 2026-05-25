module Test1.Sol where

import Data.List (intercalate, (!?), inits, tails)
import Data.Char (toLower, isSpace, toUpper)
import Control.Monad
import Control.Applicative
import Control.Monad.Free
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe
import Control.Monad.Except
import Data.Traversable
import Data.Foldable
import Control.Monad.Trans.Except (throwE, runExceptT)
import Control.Monad.Trans.State (runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
------------------------
--- 1.
------------------------
newtype AccountId = AccountId String deriving (Show, Eq, Ord)
newtype User = User String deriving (Show, Eq, Ord)
--newtype AccountId = AccountId String deriving (Show, Eq)
-- newtype User = User String deriving (Show, Eq)
data Error = InsufficientFunds | AccountNotFound deriving (Show, Eq)

data ATMF a
    = CheckBalance AccountId (Int -> a)
    | Deposit AccountId Int a
    | Withdraw AccountId Int (Either Error a)
    | GetUser AccountId (Maybe User -> a)

instance Functor ATMF where
    fmap f (CheckBalance acc next) = CheckBalance acc (f . next)
    fmap f (Deposit acc amount next) = Deposit acc amount (f next)
    fmap f (Withdraw acc amount next) = Withdraw acc amount (fmap f next)
    fmap f (GetUser acc next) = GetUser acc (f . next)

{- Leyes del Functor:
   1. Identidad (fmap id == id):
      fmap id (Deposit acc amt a) = Deposit acc amt (id a) = Deposit acc amt a
      fmap id (Withdraw acc amt (Right a)) = Withdraw acc amt (Right (id a)) = Withdraw acc amt (Right a)
   2. Composición (fmap (f . g) == fmap f . fmap g):
      fmap (f . g) (CheckBalance acc next) = CheckBalance acc ((f . g) . next)
      = CheckBalance acc (f . (g . next)) = fmap f (CheckBalance acc (g . next))
      = fmap f (fmap g (CheckBalance acc next))
-}

checkBalance :: AccountId -> Free ATMF Int
checkBalance acc = liftF (CheckBalance acc id)

deposit :: AccountId -> Int -> Free ATMF ()
deposit acc amount = liftF (Deposit acc amount ())

-- Nota: Para que el tipo sea Free ATMF (Either Error ()), envolvemos el caso de éxito
-- en Right (Right ()). El primer Right es de la estructura Either del constructor,
-- el segundo Right es el valor `a` que se devuelve en caso de éxito.

withdraw :: AccountId -> Int -> Free ATMF (Either Error ())
withdraw acc amount = liftF (Withdraw acc amount (Right (Right ())))

getUser :: AccountId -> Free ATMF (Maybe User)
getUser acc = liftF (GetUser acc id)

transfer :: AccountId -> AccountId -> Int -> Free ATMF (Either Error ())
transfer from to amount = do
    uFrom <- getUser from
    uTo <- getUser to
    case (uFrom, uTo) of
        (Just _, Just _) -> do
            bal <- checkBalance from
            if bal >= amount
                then do
                    _ <- withdraw from amount
                    deposit to amount
                    return (Right ())
                else return (Left InsufficientFunds)
        _ -> return (Left AccountNotFound)

-- No es posible conmutar los efectos. 
--La firma Either Error (Free ATMF ()) forzaría a que la validación de errores se realice de manera pura (estática) 
-- antes de la construcción del árbol de sintaxis de la mónada libre

accountIds :: Free ATMF a -> [AccountId]
accountIds (Pure _) = []
accountIds (Free step) = case step of
    CheckBalance acc next -> acc : accountIds (next 0)
    Deposit acc amt next  -> acc : accountIds next
    Withdraw acc amt next -> acc : either (const []) accountIds next
    GetUser acc next      -> acc : accountIds (next Nothing)

newtype BankState = MkBankState (Map AccountId (Int, User)) deriving (Show)

interpret :: Free ATMF a -> BankState -> (Either Error a, BankState)
interpret (Pure a) state = (Right a, state)
interpret (Free step) state@(MkBankState m) = case step of
    CheckBalance acc next ->
        let bal = case Map.lookup acc m of
                    Just (b, _) -> b
                    Nothing -> 0
        in interpret (next bal) state
        
    Deposit acc amt next ->
       case Map.lookup acc m of
        Just (b, u) ->
            let m' = Map.insert acc (b + amt, u) m
            in interpret next (MkBankState m')

        Nothing ->
            (Left AccountNotFound, state)
        
    Withdraw acc amt next ->
        case Map.lookup acc m of
            Nothing ->
                (Left AccountNotFound, state)

            Just (b, u)
                | b >= amt ->
                    let m' = Map.insert acc (b - amt, u) m
                    in case next of
                        Right n  -> interpret n (MkBankState m')
                        Left err -> (Left err, MkBankState m')

                | otherwise ->
                    (Left InsufficientFunds, state)
                
    GetUser acc next ->
        let u = fmap snd (Map.lookup acc m)
        in interpret (next u) state



-------------------------
--- 2.
-------------------------

type Weight = Int
newtype Graph a = Graph [(a,[(Weight,a)])]

neighbors :: Eq a => a -> Graph a -> [a]
neighbors node (Graph xs) =
    case lookup node xs of
        Just ns -> map snd ns
        Nothing -> []

dfs :: Ord a => Graph a -> a -> State (Set a) [[a]]
dfs graph node = do
    visited <- get

    if Set.member node visited
        then pure []
        else do
            put (Set.insert node visited)

            let ns = neighbors node graph

            result <-
                if null ns
                    then pure [[node]]
                    else do
                        subpaths <- mapM (dfs graph) ns

                        pure
                            [ node:path
                            | paths <- subpaths
                            , path <- paths
                            ]

            put visited

            pure result

paths :: forall a. Ord a => a -> Graph a -> [[a]]
paths start graph =
    evalState (dfs graph start) Set.empty

-- utilo State y no Reader porque State sirve para informacion que cambia mientras que Reader sirve para informacion inmutable compartida.
-- Puede hacerse con Reader pero sería incómodo. se tendría que pasar manualmente el conjunto actualizado, entoces realemente se desaprovecha el Reader. es como hacer una recursion maniual
-- por eso State es más adecuado para este caso.

--------------------------
--- 3.
--------------------------


data MyExceptT e m a = MyExceptT { runMyExceptT :: m (Either e a) }

instance Functor m => Functor (MyExceptT e m) where
    fmap f (MyExceptT mea) =
        MyExceptT (fmap (fmap f) mea)

instance Applicative m => Applicative (MyExceptT e m) where
    pure x =
        MyExceptT (pure (Right x))

    MyExceptT mf <*> MyExceptT mx =
        MyExceptT ((<*>) <$> mf <*> mx)

instance Monad m => Monad (MyExceptT e m) where
    MyExceptT mea >>= f =
        MyExceptT $ do
            ea <- mea

            case ea of
                Left err ->
                    pure (Left err)

                Right x ->
                    runMyExceptT (f x)

--------------------------
--- 4.
--------------------------

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)

data Color = White | Black deriving (Eq, Show)

data Piece = Piece
    { pieceType :: PieceType
    , color     :: Color
    } deriving (Eq, Show)

data Position = Position
    { rank :: Int
    , file :: Char
    } deriving (Eq,Ord, Show)
data Move = Move
    { from :: Position
    , to   :: Position
    } deriving (Eq, Show)

newtype Board = MkBoard (Map Position Piece)

data BoardState' a = BoardState'
    { board       :: Board
    , turn        :: Color
    , customState :: a
    , gameOver     :: Bool
    , winner       :: Maybe Color
    , moveHistory   :: [Move]
    }

type AdditionalState = ()
type BoardState = BoardState' AdditionalState

data BoardError
    = InvalidMove { reasonIM :: String }
    deriving (Show)

getPiece :: Board -> Position -> Maybe Piece
getPiece (MkBoard m) pos =
    Map.lookup pos m


setPiece :: Board -> Position -> Maybe Piece -> Board
setPiece (MkBoard m) pos maybePiece =
    case maybePiece of
        Just piece ->
            MkBoard (Map.insert pos piece m)

        Nothing ->
            MkBoard (Map.delete pos m)
            
isInsideBoard :: Position -> Bool
isInsideBoard (Position rank file) =
    rank >= 1 && rank <= 8 && file >= 'a' && file <= 'h'

whitePiece :: PieceType -> Piece
whitePiece pt =
    Piece pt White

blackPiece :: PieceType -> Piece
blackPiece pt =
    Piece pt Black 

initialBoard :: Board
initialBoard =
    MkBoard $ Map.fromList $

        -- Blancas
        [ (Position 1 'a', whitePiece Rook)
        , (Position 1 'b', whitePiece Knight)
        , (Position 1 'c', whitePiece Bishop)
        , (Position 1 'd', whitePiece Queen)
        , (Position 1 'e', whitePiece King)
        , (Position 1 'f', whitePiece Bishop)
        , (Position 1 'g', whitePiece Knight)
        , (Position 1 'h', whitePiece Rook)
        ]

        ++

        [ (Position 2 f, whitePiece Pawn)
        | f <- ['a'..'h']
        ]

        ++

        -- Negras
        [ (Position 8 'a', blackPiece Rook)
        , (Position 8 'b', blackPiece Knight)
        , (Position 8 'c', blackPiece Bishop)
        , (Position 8 'd', blackPiece Queen)
        , (Position 8 'e', blackPiece King)
        , (Position 8 'f', blackPiece Bishop)
        , (Position 8 'g', blackPiece Knight)
        , (Position 8 'h', blackPiece Rook)
        ]

        ++

        [ (Position 7 f, blackPiece Pawn)
        | f <- ['a'..'h']
        ]

-- Genera todas las 64 casillas del tablero (Filas 1-8, Columnas a-h)
allPositions :: [Position]
allPositions = [Position r f | r <- [1..8], f <- ['a'..'h']]

-- Encuentra la posición exacta del Rey de un color específico
findKing :: Board -> Color -> Position
findKing brd c = head 
    [ pos | pos <- allPositions
          , Just piece <- [getPiece brd pos]
          , pieceType piece == King
          , color piece == c ]

-- Función auxiliar para mover una posición por un offset (dr, df)
offsetPos :: Position -> (Int, Int) -> Position
offsetPos (Position r f) (dr, df) =
    Position (r + dr) (toEnum (fromEnum f + df))

-- Función auxiliar para verificar si una posición está vacía
isEmpty :: Board -> Position -> Bool
isEmpty board pos =
    case getPiece board pos of
        Nothing -> True
        Just _  -> False

isEnemy :: Board -> Color -> Position -> Bool
isEnemy board c pos =
    case getPiece board pos of
        Just piece -> color piece /= c
        Nothing    -> False

-- Movimientos del Peón (1 paso, 2 pasos, capturas, en passant)
pawnMoves :: BoardState -> Position -> Piece -> [Position]
pawnMoves state pos piece =
    filter isInsideBoard $
        forwardMoves ++ doubleStepMoves ++ captureMoves ++ enPassantMoves
  where
    brd = board state
    c = color piece
    
    -- Variables según el color
    dir = if c == White then 1 else -1
    startRank = if c == White then 2 else 7
    enPassantRank = if c == White then 5 else 4

    oneStep = offsetPos pos (dir, 0)
    twoSteps = offsetPos pos (dir * 2, 0)

    -- 1 paso adelante
    forwardMoves = 
        if isEmpty brd oneStep 
        then [oneStep] 
        else []

    -- 2 pasos adelante (solo desde la posición inicial y si el camino está libre)
    doubleStepMoves =
        if rank pos == startRank && isEmpty brd oneStep && isEmpty brd twoSteps
        then [twoSteps]
        else []

    -- Capturas normales en diagonal
    captureMoves =
        [ p
        | df <- [-1, 1]
        , let p = offsetPos pos (dir, df)
        , isEnemy brd c p
        ]

    -- Comer al paso (En Passant)
    enPassantMoves = case moveHistory state of
        (Move lastFrom lastTo : _) ->
            [ offsetPos pos (dir, df)
            | df <- [-1, 1]
            , rank pos == enPassantRank -- Nuestro peón debe estar en la fila 5 (Blanco) o 4 (Negro)
            , let enemyPos = offsetPos pos (0, df) -- La posición del peón enemigo a nuestro lado
            , lastTo == enemyPos -- El último movimiento del enemigo terminó ahí
            , file lastFrom == file lastTo -- El enemigo se movió en línea recta
            , abs (rank lastFrom - rank lastTo) == 2 -- El enemigo hizo un salto doble
            , case getPiece brd enemyPos of
                Just (Piece Pawn enemyColor) -> enemyColor /= c
                _ -> False
            ]
        _ -> []

-- Función auxiliar para verificar si una posición tiene una pieza enemiga
knightMoves :: Board -> Position -> Piece -> [Position]
knightMoves board pos piece =
    filter validTarget candidates
  where
    offsets =
        [ (2,1), (2,-1)
        , (-2,1), (-2,-1)
        , (1,2), (1,-2)
        , (-1,2), (-1,-2)
        ]

    candidates =
        [ offsetPos pos off
        | off <- offsets
        , isInsideBoard (offsetPos pos off)
        ]

    validTarget p =
        case getPiece board p of
            Nothing -> True
            Just other -> color other /= color piece

-- Función auxiliar para deslizar una pieza en una dirección (dr, df)
slide :: Board -> Color -> Position -> (Int, Int) -> [Position]
slide board c pos dir =
    let nextPos = offsetPos pos dir
    in if not (isInsideBoard nextPos)
        then []
        else case getPiece board nextPos of
            Nothing -> nextPos : slide board c nextPos dir -- Casilla vacía, sigue avanzando
            Just p  -> if color p /= c
                       then [nextPos] -- Enemigo: lo puede capturar pero ahí se detiene
                       else []        -- Aliado: el camino está bloqueado

-- Movimientos del Alfil (diagonales)
bishopMoves :: Board -> Position -> Piece -> [Position]
bishopMoves board pos piece =
    let dirs = [(1,1), (1,-1), (-1,1), (-1,-1)]
    in concatMap (slide board (color piece) pos) dirs

-- Movimientos de la Torre (ortogonales)
rookMoves :: Board -> Position -> Piece -> [Position]
rookMoves board pos piece =
    let dirs = [(1,0), (-1,0), (0,1), (0,-1)]
    in concatMap (slide board (color piece) pos) dirs

-- Movimientos de la Reina (diagonales + ortogonales)
queenMoves :: Board -> Position -> Piece -> [Position]
queenMoves board pos piece =
    let dirs = [(1,1), (1,-1), (-1,1), (-1,-1), (1,0), (-1,0), (0,1), (0,-1)]
    in concatMap (slide board (color piece) pos) dirs            


-- Movimientos del Rey (1 paso en cualquier dirección + Enroque)
kingMoves :: BoardState -> Position -> Piece -> [Position]
kingMoves state pos piece =
    filter validTarget candidates ++ castlingMoves
  where
    brd = board state
    c = color piece
    dirs = [(1,1), (1,-1), (-1,1), (-1,-1), (1,0), (-1,0), (0,1), (0,-1)]
    
    candidates = 
        [ offsetPos pos dir 
        | dir <- dirs
        , isInsideBoard (offsetPos pos dir) 
        ]
        
    validTarget p = 
        case getPiece brd p of
            Nothing -> True
            Just other -> color other /= c

    -- Lógica del Enroque
    castlingMoves = 
        let rankK = if c == White then 1 else 8
            posE = Position rankK 'e'
            posF = Position rankK 'f'
            posG = Position rankK 'g'
            posD = Position rankK 'd'
            posC = Position rankK 'c'
            posB = Position rankK 'b'
            
            -- Verificamos si en el historial de movimientos la posición inicial aparece como "origen"
            kingMoved  = any (\(Move f _) -> f == posE) (moveHistory state)
            rookHMoved = any (\(Move f _) -> f == Position rankK 'h') (moveHistory state)
            rookAMoved = any (\(Move f _) -> f == Position rankK 'a') (moveHistory state)
            
            -- Función clave: simula mover el rey a una casilla y verifica si queda en jaque
            isSafe p = not (isInCheck (simulateMove state posE p) c)
            
            -- Enroque Corto: Vacío, sin jaque actual, y F y G seguras
            canCastleShort = not kingMoved && not rookHMoved 
                             && isEmpty brd posF && isEmpty brd posG
                             && not (isInCheck state c) 
                             && isSafe posF && isSafe posG

            -- Enroque Largo: Vacío, sin jaque actual, y D y C seguras (B no importa si está atacada)
            canCastleLong  = not kingMoved && not rookAMoved 
                             && isEmpty brd posB && isEmpty brd posC && isEmpty brd posD
                             && not (isInCheck state c)
                             && isSafe posD && isSafe posC
                             
        in (if canCastleShort then [posG] else []) ++
           (if canCastleLong  then [posC] else [])

-- Función auxiliar para generar movimientos en línea (como los de torre, alfil, reina)
validMoves :: BoardState -> Position -> [Position]
validMoves state pos =
    let brd = board state
    in case getPiece brd pos of
        Nothing -> []
        Just piece ->
            case pieceType piece of
                Pawn   -> pawnMoves state pos piece
                Knight -> knightMoves brd pos piece
                Bishop -> bishopMoves brd pos piece
                Rook   -> rookMoves brd pos piece
                Queen  -> queenMoves brd pos piece
                King   -> kingMoves state pos piece

isValidMove :: BoardState -> Move -> Bool
isValidMove state (Move fromPos toPos) =
    case getPiece (board state) fromPos of
        Nothing -> False
        Just piece ->
            -- 1. Debe ser el turno del color de la pieza
            color piece == turn state &&
            -- 2. El destino debe estar en los movimientos posibles de la pieza
            toPos `elem` (validMoves state fromPos) &&
            -- 3. No debe dejar al propio Rey en jaque (Regla anti-suicidio)
            not (isInCheck (simulateMove state fromPos toPos) (color piece))

-- Verifica si el jugador de un color no tiene ningún movimiento legal posible
hasNoLegalMoves :: BoardState -> Color -> Bool
hasNoLegalMoves state c =
    let allMyPieces = [ pos | pos <- allPositions
                            , Just p <- [getPiece (board state) pos]
                            , color p == c ]
        -- Intentamos encontrar AL MENOS un movimiento que sea válido
        possibleMoves = [ Move from to | from <- allMyPieces
                                       , to <- validMoves state from ]
    in not $ any (isValidMove state) possibleMoves

-- Verifica si quedan tan pocas piezas que nadie puede ganar
isInsufficientMaterial :: Board -> Bool
isInsufficientMaterial (MkBoard m) = 
    Map.size m <= 2  -- Solo quedan los dos reyes


-- Función para detectar Tablas (Empate)
isStalemate :: BoardState -> Color -> Bool
isStalemate state c = 
    not (isInCheck state c) && hasNoLegalMoves state c

-- Verifica si el Rey de un color está siendo atacado
isInCheck :: BoardState -> Color -> Bool
isInCheck state c =
    let brd = board state
        kingPos = findKing brd c
        
        -- Encontramos dónde están todos los enemigos
        enemyPositions = [ pos | pos <- allPositions
                               , Just piece <- [getPiece brd pos]
                               , color piece /= c ]
                               
    -- ¿Algún enemigo puede moverse a la posición de nuestro Rey?
    in any (\p -> kingPos `elem` validMoves state p) enemyPositions

isCheckmate :: BoardState -> Color -> Bool
isCheckmate state c =
    isInCheck state c && hasNoLegalMoves state c

-- Simula un movimiento solo en el tablero para ver el futuro, sin reglas extra
simulateMove :: BoardState -> Position -> Position -> BoardState
simulateMove state from to =
    let brd = board state
        piece = getPiece brd from
        -- Lo quitamos del origen y lo ponemos en el destino
        brd' = setPiece (setPiece brd from Nothing) to piece
    in state { board = brd' }

initialState :: BoardState
initialState =
    BoardState'
        { board = initialBoard
        , turn = White
        , customState = ()
        , gameOver = False
        , winner = Nothing
        , moveHistory = []
        }
-- Función para convertir una pieza a su representación de carácter
pieceChar :: Piece -> Char
pieceChar (Piece Pawn White)   = 'P'
pieceChar (Piece Knight White) = 'N'
pieceChar (Piece Bishop White) = 'B'
pieceChar (Piece Rook White)   = 'R'
pieceChar (Piece Queen White)  = 'Q'
pieceChar (Piece King White)   = 'K'

pieceChar (Piece Pawn Black)   = 'p'
pieceChar (Piece Knight Black) = 'n'
pieceChar (Piece Bishop Black) = 'b'
pieceChar (Piece Rook Black)   = 'r'
pieceChar (Piece Queen Black)  = 'q'
pieceChar (Piece King Black)   = 'k'

--  Función para imprimir el tablero en formato legible
toString :: Board -> String
toString board =
    unlines $
        [ show r ++ " " ++ unwords [ [squareChar (Position r f)] | f <- ['a'..'h'] ]
        | r <- [8,7..1]
        ]
        ++
        ["  a b c d e f g h"]
  where
    squareChar pos =
        case getPiece board pos of
            Just piece -> pieceChar piece
            Nothing    -> '.'


move :: Move -> StateT BoardState (ExceptT BoardError IO) ()
move mv@(Move fromPos toPos) = do 
    state <- get
    let currentBoard = board state
        currentPlayer = turn state

    -- 1. Validar el movimiento completo
    if not (isValidMove state mv)
    then lift $ throwE (InvalidMove "Movimiento ilegal: No sigue las reglas o dejas al Rey en jaque.")
    else do
        -- Obtenemos la pieza (sabemos que existe porque isValidMove dio True)
        let piece = fromJust (getPiece currentBoard fromPos) 

        -- A) Lógica En Passant
        let isEnPassant = pieceType piece == Pawn 
                          && file fromPos /= file toPos 
                          && isEmpty currentBoard toPos
            capturedPawnPos = Position (rank fromPos) (file toPos)
            boardSinEnemigo = if isEnPassant 
                              then setPiece currentBoard capturedPawnPos Nothing
                              else currentBoard

        -- B) Lógica Coronación (Manual)
        let isPromotion = pieceType piece == Pawn 
                          && (rank toPos == 8 || rank toPos == 1)
        
        piezaDestino <- if isPromotion 
                        then liftIO $ do
                            putStrLn "¡Coronación! Elige pieza (Q: Reina, R: Torre, B: Alfil, N: Caballo):"
                            choice <- getLine
                            let pType = case map toUpper choice of
                                    "R" -> Rook
                                    "B" -> Bishop
                                    "N" -> Knight
                                    _   -> Queen 
                            return $ Piece pType currentPlayer
                        else return piece

        -- C) Actualizar Tablero Base
        let boardSinOrigen = setPiece boardSinEnemigo fromPos Nothing
            nuevoBoard     = setPiece boardSinOrigen toPos (Just piezaDestino)

        -- D) Lógica Enroque (Mover la torre automáticamente)
        let isCastling = pieceType piece == King && abs (fromEnum (file fromPos) - fromEnum (file toPos)) == 2
            boardFinal = if isCastling
                         then let rankK = rank fromPos
                                  (rFO, rFD) = if file toPos == 'g' then ('h', 'f') else ('a', 'd')
                                  rookFrom = Position rankK rFO
                                  rookTo   = Position rankK rFD
                                  rookP    = getPiece nuevoBoard rookFrom
                              in setPiece (setPiece nuevoBoard rookFrom Nothing) rookTo rookP
                         else nuevoBoard

        -- 5. CAMBIAR EL TURNO (Explícito)
        let siguienteTurno = if currentPlayer == White then Black else White

        -- 6. Guardar el nuevo estado completo
        put state 
            { board = boardFinal
            , turn = siguienteTurno
            , moveHistory = mv : moveHistory state
            }

parseMove :: String -> Either BoardError Move
parseMove input =
    let cleaned = map toLower (filter (not . isSpace) input)
    in case cleaned of       
        [f1, r1, '-', f2, r2]
            | isValid f1 r1 && isValid f2 r2 ->
                Right $ Move (Position (read [r1]) f1) (Position (read [r2]) f2)
                
        _ -> Left (InvalidMove $ "Formato no reconocido. Usa notación como 'e2-e4'.")
  where
    isValid f r = f `elem` ['a'..'h'] && r `elem` ['1'..'8']

-- Función principal del juego, maneja el ciclo de turnos y la interacción con el usuario
playGame :: StateT BoardState (ExceptT BoardError IO) ()
playGame = do
    currentState <- get
    let currentPlayer = turn currentState
        check = isInCheck currentState currentPlayer
        noMoves = hasNoLegalMoves currentState currentPlayer
        insufficient = isInsufficientMaterial (board currentState)

    -- 1. Verificar condiciones de fin de juego (Mate o Ahogado)
    if insufficient
    then do
        liftIO $ putStrLn $ "\n" ++ toString (board currentState)
        liftIO $ putStrLn "¡TABLAS! Insuficiencia de material (Rey vs Rey)."
        return ()
    else if noMoves
    then if check
         then do -- Caso: Sin movimientos + En Jaque = MATE
            let ganador = if currentPlayer == White then "Negras" else "Blancas"
            liftIO $ putStrLn $ "\n" ++ toString (board currentState)
            liftIO $ putStrLn $ "¡JAQUE MATE! Han ganado las " ++ ganador ++ "."
            return () 
         else do -- Caso: Sin movimientos + Sin Jaque = AHOGADO
            liftIO $ putStrLn $ "\n" ++ toString (board currentState)
            liftIO $ putStrLn "¡TABLAS POR AHOGADO! El juego termina en empate."
            return ()
        

    else do
        -- 2. Si el juego sigue, avisar si hay Jaque normal
        when check $ liftIO $ putStrLn "\n¡ESTAS EN JAQUE! Debes proteger a tu Rey."

        -- 3. Mostrar información del turno y pedir movimiento
        liftIO $ putStrLn "\n----------------------------------------"
        liftIO $ putStrLn $ toString (board currentState)
        liftIO $ putStrLn $ "Turno de: " ++ show currentPlayer
        liftIO $ putStrLn "Ingresa tu movimiento (ej. e2-e4) o escribe 'salir':"
        
        -- 4. Leer la entrada del usuario
        input <- liftIO getLine
        
        if input == "salir"
            then liftIO $ putStrLn "¡Gracias por jugar! Hasta la próxima."
            else case parseMove input of
                Left err -> do
                    liftIO $ print err
                    playGame -- Repetir turno por error de formato
                    
                Right m -> do
                    -- Intentamos ejecutar el movimiento
                    resultado <- tryMove m
                    
                    case resultado of
                        Left err -> do
                            liftIO $ putStrLn "¡Movimiento inválido!"
                            liftIO $ print err
                            playGame -- Repetir turno por movimiento ilegal
                        Right _ -> do
                            liftIO $ putStrLn "¡Movimiento aceptado!"
                            playGame -- Siguiente turno
  where
    -- Función auxiliar para capturar el error de 'move' sin romper la mónada
    tryMove m = catchError (move m >> return (Right ())) (return . Left)
             

playChess :: IO ()
playChess = runExceptT (runStateT playGame initialState) >>= \case
    Left err -> putStrLn $ "Game ended with error: " ++ show err
    Right _ -> putStrLn "Game ended successfully"

