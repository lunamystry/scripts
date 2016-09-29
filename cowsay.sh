
#!/bin/bash

# The following can be added to the end of ~/.bashrc or /etc/bash.bashrc
# to generate random, amusing welcome text with each logon.

# Check for interactive terminal (should not produce output otherwise).
#if [ "$PS1" ]; then
  # Set location of cow files.
  cowdir="/usr/share/cows"

  # Create an array from the files in our $cowdir.
  cows=($cowdir/*)

  # Randomly select a cow from our cows 
  # (${#myarr[*]} == num items in array, RANDOM == random number).
  mycow="${cows[RANDOM % ${#cows[*]}]}"
  # mycow="$cowdir/bunny.cow"

  # Create an array of possible cow actions (think or say).
  cowactions=("cowsay" "cowthink")

  # Randomly select an action.
  myaction="${cowactions[RANDOM % ${#cowactions[*]}]}"
  # myaction="cowsay"

  # Call fortune and pass the results to cowthink/cowsay with our chosen cow.
  fortune | $myaction -n -f "`basename ${mycow}`"
#fi
