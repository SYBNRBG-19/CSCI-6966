module ReportGenerator (generateReport) where

import Types

generateReport :: [Vulnerability] -> IO ()
generateReport vulns = do
  putStrLn "Vulnerability Report:"
  mapM_ printVuln vulns

printVuln :: Vulnerability -> IO ()
printVuln vuln = do
  putStrLn $ "Type: " ++ vulnType vuln
  putStrLn $ "Description: " ++ description vuln
  putStrLn $ "Location: " ++ location vuln
  putStrLn ""
