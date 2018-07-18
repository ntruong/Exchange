--------------------------------------------------------------------------------
module SpecRunner
  ( runner
  ) where
--------------------------------------------------------------------------------

runner :: String -> [(String, Bool)] -> IO ()
runner specName specs = result
  where
    formatSpec :: (String, Bool) -> IO ()
    formatSpec (specID, spec)
      | spec      = return ()
      | otherwise = putStrLn $ "[!] Failed " ++ specName ++ "/" ++ specID
    result = sequence_ $ putStrLn "" : (formatSpec <$> specs)
