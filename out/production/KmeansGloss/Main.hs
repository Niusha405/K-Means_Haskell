import System.IO
import Control.Monad
import Data.List      (sortBy, unwords)
import Data.Function  (on)
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (maybe)
import System.IO.Unsafe
import System.Directory.Internal.Prelude (zip)
import Data.Text.Unsafe

type Point2D = [Float]
type VecPoint=[Point2D]
readPoints :: [Char] -> [Point2D]
readPoints contents =result
  where
       points = map words (lines contents)
       result=map (\x -> map (\y -> read y :: Float) x) points


list_random :: Int->Int ->  IO[Int]
list_random n reng= sequence $ replicate n $ randomRIO (0,reng::Int)
zip_s :: [Int] -> [Int] -> [String]
zip_s (a:as) (b:bs) = str: zip_s as bs
  where str = show a ++" "++show b
zip_s _      _      = []

rendom_txt::Handle->Int->IO()
rendom_txt handel num=do

  let lis_x=list_random num 10
      lis_y=list_random num 10
      lx=inlinePerformIO lis_x
      ly=inlinePerformIO lis_y
      zip_list=zip_s  lx ly

  mapM_ (\point->hPutStrLn handel (point)) zip_list
--



x=(!!0)
y=(!!1)

euclidean_distance::Point2D->Point2D->Float
euclidean_distance point1 point2=sqrt (x_d+y_d)
  where
    x_d=(x point1-x point2)**2
    y_d=(y point1-y point2)**2


loopmin::Int->[Float]->Float->Int
loopmin ind (l:xs) min=if l==min then ind+1 else loopmin (ind+1) xs min
getMinIndex :: [Float]->Int
getMinIndex xs =
  loopmin 0 xs min
  where min=minimum xs

--loop::Int->VecPoint->Point2D->Int
--loop ind (l:xs) p=if x l==x p && y l==y p then ind+1 else loop (ind+1) xs p
--getindex::VecPoint->Point2D->Int
--getindex points p =loop 0 points p

chek::Point2D->Point2D->Bool
chek p q=x q==x p && y q==y p
find::VecPoint->[Int]->Point2D->Int
find points indexs p=if chek (head points) p then head indexs
                            else find (tail points) (tail indexs) p                        

getClosestCentroid :: Point2D -> VecPoint -> Int
getClosestCentroid point centroids =
  getMinIndex distances
  where distances = map (euclidean_distance point) centroids

getCenter :: VecPoint -> Point2D
getCenter points = [average_x,average_y]
           where
             x_points=map x points
             average_x= sum x_points / fromIntegral (length x_points) :: Float

             y_points=map y points
             average_y= sum y_points / fromIntegral (length y_points) :: Float
center_new::Int->[Int]->VecPoint->Point2D
center_new ind indexs points=
  let ind_point=zip indexs points
      list_point_ind=[snd ind_p | ind_p<-ind_point,(fst ind_p)==ind ]
  in getCenter list_point_ind
--  in print $ list_point_ind


classifyPoints :: VecPoint -> VecPoint -> [Int]
classifyPoints points centroids = map ((flip getClosestCentroid) centroids) points

kmeans_iterate::VecPoint->VecPoint->VecPoint
kmeans_iterate points centroids=new_centroids
  where indexs   = classifyPoints points centroids
        new_centroids=map ( \ind -> center_new ind indexs points) [1..maximum indexs]

iterate_k_means::VecPoint->VecPoint->Int->[Int]
iterate_k_means data_points centroids total_iteration=result 
        
        where  newCentroids=(iterate (kmeans_iterate data_points) centroids) !! (total_iteration - 1)
               result= classifyPoints data_points newCentroids

plus::Float->Float->Float
plus a b= a+b

data PointCoordinate = PointCoordinate Float Float deriving Show

window :: Display
window = InWindow "K-means" (700, 700) (1, 1)

background :: Color
background = white

colors :: [ Color ]
colors= [ red, orange,blue, violet, magenta, yellow, chartreuse, green, aquamarine, cyan, azure, rose]


makePicture :: Point2D->VecPoint->[Int]->[Color] -> Picture
makePicture p points indexs colors
             = Translate  s t $Color (withAlpha 0.9 c) $ circleSolid 0.1 
             where 
                s=x p 
                t=y p
                c=colors!!find points indexs p
                
                                             

drow::VecPoint->[Int]->[Color]->Picture
drow points indexs colors=pictures(map ( \p -> makePicture p points indexs colors) points)




main::IO()
main=do

  handle_write <- openFile "test.txt" WriteMode
  rendom_txt handle_write 100
  hClose handle_write
--  print$ write_random_point 100
  handle_read <- openFile "test.txt" ReadMode
  contents <- hGetContents handle_read
  let points = readPoints contents
  print points
  hClose handle_read
  let k=10 --number of cluster
      center=take k points

  let indexs=iterate_k_means points center 1000
  print indexs
  display window background $drow points indexs colors


  