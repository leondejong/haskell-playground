import qualified IdSpec
import qualified OptionalSpec
import qualified OrSpec
import qualified TupleSpec
import qualified ListSpec
import qualified InputSpec
import qualified OutputSpec
import qualified StoreSpec

main :: IO ()
main = do
  IdSpec.runQc
  OptionalSpec.runQc
  OrSpec.runQc
  TupleSpec.runQc
  ListSpec.runQc
  InputSpec.runQc
  OutputSpec.runQc
  StoreSpec.runQc
