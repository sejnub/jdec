## Status and rights

This module does its job but there is a lot which could (but probably won't) be improved.

Do with it whatever you want. I claim no rights to it.


## Motivation
Parsing JSON in erlang is IMHO not at all a straightforward thing to do. 
I wanted to have something that is similar to the javascript syntax of simply saying "jsonString.member.submember".

Therefore I wrote this parser which allows you to access a member of a JSON object by simply saying:
    jdec:get(JsonString, ".member.submember").

Additionally I added a few related functions to make the access even more comfortable.


## Release process

1) Copy the files to your project


## Usage


    JSON1 = {
              \"meta_schema_name\": \"real\",
              \"meta_schema_version\": 1,
              \"comments\": [
                \"Bernhard\",
                \"Tina\"
              ],
              \"identifier\": {
                \"asset\": 45,
                \"aspect\": 12
              },
              \"payload\": {
                \"payload_schema_name\": \"real\",
                \"payload_schema_version\": 1,
                \"id\": 123,
                \"time\": \"2011-05-14T14:45:12.435691\",
                \"data\": {
                  \"real\": 23.56456
                }
              }
            }.
    
    
    jdec:get(  JSON1, ".identifier.asset") -> [45] 
    jdec:get_s(JSON1, ".identifier.asset") -> 45 



* Have a look at the *_test functions in jdec.erl for further examples


## Comments

* This module is not optimized for speed.

* mochijson.erl is included as the JSON-Decoder and does most of the work. 
  I justed added the parser for the dot syntax.
                                                    

                                                     
