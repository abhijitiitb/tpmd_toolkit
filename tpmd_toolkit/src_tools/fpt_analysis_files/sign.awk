#!/usr/bin/awk -f

#==============================================================================================
#
#   Copyright 2020 Abhijit Chatterjee
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#==============================================================================================

function sign(x) { 
   return x < 0 ? -1 : x > 0 
} 
function abs(x) {
   return x*sign(x)
}
{
( abs($1) < BoxX/2 ) ? dx=$1 : dx=$1-BoxX*sign($1)
( abs($2) < BoxY/2 ) ? dy=$2 : dy=$2-BoxY*sign($2)
( abs($3) < BoxZ/2 ) ? dz=$3 : dz=$3-BoxZ*sign($3)
print dx" "dy" "dz
}
