## Motivation

Decoding JSON in erlang is (IMHO) not at all a straightforward thing to do. I wanted to have something that is similar to the javascript syntax of simply saying "jsonVariable.member.submember".




## Release process

This document simply outlines the release process:

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

* This module is not optimized for speed by now.

* mochijson.erl is included as the JSON-Decoder and does most of the work. 
  I justed added the parser for the dot syntax.
