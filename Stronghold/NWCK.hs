module NWCK where

import Data.Char
import Data.Attoparsec.Text hiding (take)
import Data.Attoparsec.Combinator
import qualified Data.Text as T
import Data.Tree
import Control.Applicative

import Control.Monad.State
import Data.Traversable (traverse)

uniqueLabels:: NWTree -> NWTree
uniqueLabels t = head $ uniqueLabelsF [t]

uniqueLabelsF::Forest (T.Text,Integer) -> Forest (T.Text,Integer)
uniqueLabelsF f = evalState (mapM globalAction f) 0
    where
        globalAction t = unwrapMonad $ traverse localAction t
        localAction (t,v) = WrapMonad $ do
            i <- get
            put (i+1)
            let newValue = if t == internalStdLabel then (T.pack (show i),v) else (t,v)
            return newValue 

stdWeight::Integer
stdWeight = 1

ts::T.Text
ts = T.pack "((,,),,castor)root;"

fromRight (Right s) = s
fromRight (Left _)  = error "You said you were sure, it's not my fault!"

sample1 = T.pack "((Acanthoscurria_saiga,(Ephibolus_gebleri,Leptobrachium_squamatus)),(((((((((Acanthoscurria_unicolor,(Chlamydotis_penelope,Saga_angustirostris)),Riparia_ciliatus),Mabuya_collectivus),(((((((((((((((((((((((Anthropoidae_fissipes,((Buthacus_weberi,(Ophisops_vittatus,Pandinus_acuta)),(((Pachydactylus_scalaris,crecca_brachydactyla),Riparia_krueperi),Pica_coelestinus))),Underwoodisaurus_catenifer),Regulus_alpina),Aphonopelma_caudata),(Lepidobatrachus_decorus,Scolopendra_triangulum)),Apus_chuatsi),(Gyps_leucocephala,Telescopus_canorus)),((Chlamydotis_tenuirostris,(Homopholis_ibis,Phrynohyas_alba)),Hydrochelidon_rutilans)),((Aquila_sibiricus,Seokia_sauromates),Certhia_barbata)),Moschus_bimaculata),(Dipus_corone,((Leptobrachium_avicularia,(((Leptopelis_striatus,Pagophila_nigra),(Morelia_sudanensis,Thymallus_brongersmai)),Rhacodactylus_rapax)),Ziphius_temminskii))),Lagenorhynchus_epops),(Hottentotta_graculus,(Prunella_flavomaculatus,Tropidurus_falcipennis))),Larus_glaucescens),Dafila_keyzerlingii),(Rhinolophus_atthis,Squaterola_javanica)),Citharacanthus_ochropus),Ctenosaura_altaicus),(((((Avicularia_dentatus,(Nyctixalus_alpinus,Vulpanser_viridescens)),Pseudorca_gratiosa),(((Dipus_truncatus,((((Litoria_krueperi,(Odobenus_lavaretus,Rhesus_vertebralis)),Thymallus_pygargus),Nemorhaedus_collectivus),((Otocoris_jubata,Physignathus_licin),Testudo_tristis))),Pandinus_multituberculatus),(Lamprolepis_subglobosa,((Myotis_tetrix,Rissa_cinaedus),Tetrao_scabra)))),Phoca_lepturus),Bos_mlokosiewiczi)),Apodora_kazanakowi),Trachemys_gibbosus),(Eurynorhynchus_constrictor,(Onychodactylus_lutris,Theloderma_parreyssi))),Panthera_marcianus)),Caiman_schokari),(Mesoplodon_longipennis,Varanus_floridana)),Chelus_quinquetaeniata),((((Anthropoides_celer,(Megaloperdix_taezanowskyi,Minipterus_morinellus)),Chrttusia_crassidens),Rhabdophis_limosa),Phrynops_leschenaultii)),Aquila_multituberculatus),((Anolis_filipjevi,Boiga_zagrosensis),(Mustela_arenarius,Streptopelia_turtur)));"
s1a = T.pack "Pica_coelestinus"
s1b = T.pack "Riparia_ciliatus"

sample2 = T.pack "(Abantias_leucophyllata,((((((((((((((((Acanthoceros_maurus,(((((((((((((Ahaetulla_pusilla,Nemorhaedus_brongersmai),((((((Alaus_porphyrio,sibiricus_nivalis),(Madagascarophis_decorus,Tetrao_fuliginosus)),Apus_iguana),Chelydra_atrigularis),Podoces_gordoni),(((Pandion_dennysii,Phoca_alpestris),Tetraogallus_tolai),Pseudorca_vittatus))),Recurvirostra_tentaculatum),Lycodon_buccata),(Castor_monilis,Kaloula_vipio)),Anthropoidae_aristotelis),Philomachus_rusticolus),Pratincola_saxatilis),Tiliqua_porphyrio),(Megaptera_belliana,Phrynosoma_veredus)),(Emydura_baeri,Telescopus_duplex)),((((((((Alaus_tinctorius,Lampropeltis_leptochelis),Pusa_compactus),((((Brachypelma_ladogensis,Vulpanser_bicoloratum),(Equus_borealis,Hemiscorpius_glacialis)),Trapelus_saxatilis),Emydura_giganteus)),((Anas_nigropalmatus,Onychodactylus_leucomelas),Gambelia_personata)),(((((Castor_nigra,Pelomedusa_keyzerlingii),Spermophilus_mnemosyne),Fulica_pugnax),Moschus_plathyrhychos),crecca_licin)),(Aphonopelma_guentheri,((Aplopeltura_zagrosensis,(Ethmostigmus_vulpes,Mustela_minutus)),Lanius_pallasii))),Ovis_cachinans),(Minipterus_gordoni,Xenopeltis_rubicola))),Butastur_brachydactyla)),Capra_citrsola),(Colaeus_doriae,Querquedula_fuellebornii)),((Burhinus_schrencki,Cardiocranius_mutabilis),Eirenis_medici)),Phelsuma_boulengeri),((Branta_hasselquistii,Mareca_salvator),((Glareola_geyri,Halichoerus_niloticus),Neolycaena_tatarica))),(Felis_diadema,Liasis_lineatus)),Sphenurus_sieboldii),Callipogon_caeruleus),sibiricus_ferox),Anodonta_flavirufa),((Mareca_apus,Mogera_sebae),Myotis_melleri)),Laudakia_constricticollis),(((Arenaria_ladogensis,Hyla_brongersmai),Norops_wogura),Buteo_anatina)),((Chen_solitaria,Morelia_piscator),Phormictopus_dendrophila)),(Coregonus_conicus,Phrynocephalus_pendulinus)),Accipiter_himantopus);"
s2a = T.pack "Castor_nigra"
s2b = T.pack "Madagascarophis_decorus"

rootStdLabel::T.Text
rootStdLabel = T.pack "*"

internalStdLabel::T.Text
internalStdLabel = T.pack "^"

-- NEWICK Tree parsing -- 
type RPath  = [(T.Text,Integer)]
type NWTree = Tree (T.Text,Integer)

parseNWK:: T.Text -> Either String NWTree
parseNWK = parseOnly nwTree

nwTree = do
    forest <- descendant_list
    rl <- option (rootStdLabel) label
    rL <- option stdWeight edgeLength
    char ';'
    return $ Node (rl,rL) forest

descendant_list = do
    char '('
    st <- subtree `sepBy` (char ',')
    char ')'
    return st

subtree = internal <|> leaf

-- Working -- 
edgeLength = do
    char ':'
    n <- signed decimal
    return n

label = do
    l <- takeWhile1 $ inClass "_0-9a-zA-Z"
    return l

-- Working -- 
internal = do
    forest <- descendant_list 
    nl <- option internalStdLabel label
    nL <- option stdWeight edgeLength
    return $ Node (nl,nL) forest

-- Working --
leaf = do
    nl <- option internalStdLabel label
    nL <- option stdWeight edgeLength
    return $ Node (nl,nL) []

-- [START] Path finding --
pathFromRoot::NWTree -> T.Text -> Maybe RPath
pathFromRoot t p = fromRoot p [(t,[])]

fromRoot:: T.Text -> [(NWTree,RPath)] -> Maybe RPath
fromRoot l treePathList
    | null treePathList   = Nothing
    | Left p <- srcResult = Just p
    | Right newTreePathList <- srcResult = fromRoot l newTreePathList 
        where
            srcResult = foldr (\(t,path) c -> either (Left) (\e -> fmap (++ e) (find l t path)) c) (Right []) treePathList

find:: T.Text -> NWTree -> RPath -> Either RPath [(NWTree,RPath)]
find l t path
    | fst nL == l = Left $ nL:path
    | otherwise = Right $ map (\x -> (x,nL:path)) $ subForest t
        where
            nL = rootLabel t
-- Returns nothing if either A or B does not exist.      
pathFromAtoB:: NWTree -> T.Text -> T.Text -> Maybe RPath
pathFromAtoB t a0 b0
    | Just a <- pathFromRoot t a0 , Just b <- pathFromRoot t b0 = Just $ path (reverse a) (reverse b)
    | otherwise = Nothing

path::RPath -> RPath -> RPath
path [] []  = []
path []  p  = p
path p  []  = p
path a@(x:xs) b@(y:ys)
    | x /= y = a ++ b
    | otherwise = path xs ys

pathCost::RPath -> Integer
pathCost p  = foldr ((+) . snd ) 0 p
-- [END] Path finding --
