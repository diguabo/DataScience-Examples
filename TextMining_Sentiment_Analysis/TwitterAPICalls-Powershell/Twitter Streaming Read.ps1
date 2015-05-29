function Invoke-CreateNewTweetsFile 
{
#FileNameFormat
$filepath="Z:\data\TwitterStream\CollectedTweets"
$filename="Tweets_UK_Pizza_"
$timestamp = (Get-Date).AddHours(1).ToString('yyyyMMddHHmm') #Adjusted for UTC to GMT
#Create New file
$file = New-Item -ItemType file "$filepath\$filename$timestamp" -Force
return $file
}


#Import module from current script directory
Import-Module "$PSScriptRoot\InvokeTwitterAPIs.psm1"

$OAuth = @{
            'ApiKey' = Read-Host "Please Provide Twitter API Key" 
            'ApiSecret' = Read-Host "Please provide Twitter API Secret"
            'AccessToken' = Read-Host "Please provide Twitter Access Token"
            'AccessTokenSecret' = Read-Host "Please provide Twitter Access Token Secret"
         }

#keep tweets counter
$tweetcount=0


#create a new start file
$file =Invoke-CreateNewTweetsFile
#Open a Stream Writer to file (fastest write  option)
$stream = New-Object System.IO.StreamWriter($file)


#Initialise Start Time for File Splitting
$StartTime =Get-Date

 #Call API to Capture Tweets
Invoke-ReadFromTwitterStream -OAuthSettings $OAuth `
-ResourceURL 'https://stream.twitter.com/1.1/statuses/filter.json' `
-RestVerb 'POST' `
-Parameters @{'locations' = '-9.05, 48.77, 2.19, 58.88';'track'='pizza';'stall_warnings'='true'} `
-MinsToCollectStream 120 | foreach {
                                     #Calculate Elapsed Mins since Start 
                                     $CurrentTime=Get-Date
                                     $TimeDiff = New-TimeSpan $StartTime $CurrentTime      
                                     $ElapsedMins = $TimeDiff.Minutes

                                     #Cycle new file every 10 mins
                                     if($ElapsedMins -gt 10)
                                         {  
                                            $stream.flush() #Flush bits in buffer for previous batch
                                            $stream.close() #Close filestream for previous batch
                                        
                                            #create new split file and get a handle for stream writer
                                            $file =Invoke-CreateNewTweetsFile                
                                            Write-Host("Create new file :$file")
                                            $stream = New-Object System.IO.StreamWriter($file)

                                            $StartTime =Get-Date #Reset Start time for this iteration
                                         }
                                               $tweetcount+=1           
                                               $stream.WriteLine($_)    #Write Tweet JSON to file             
                                               Write-Host (" Total Tweets Streamed untill now: $tweetcount")
                                    } 
#Wrap up 
$stream.flush() #Flush bits in buffer
$stream.close() #Close  last file stream
Write-Host("Total Tweets Collected : $tweetcount")