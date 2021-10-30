{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Devtools.Schema where

import Data.Aeson.Schema

type DevtoolsSchema = [schema|
    {
        version: {
            major: Text,
            minor: Text
        },
        domains: List { 
            domain: Text,
            description: Maybe Text,
            experimental: Maybe Bool,
            deprecated: Maybe Bool,
            dependencies: Maybe (List Text),
            types: Maybe (List {
                id: Text,
                description: Maybe Text,
                type: Text,
                enum: Maybe (List Text),
                items: Maybe ({
                    $ref: Maybe Text,
                    type: Maybe Text
                }),
                properties: Maybe (List {
                    name: Text,
                    description: Maybe Text,
                    deprecated: Maybe Bool,
                    optional: Maybe Bool,
                    $ref: Maybe Text,
                    type: Maybe Text,
                    items: Maybe ({
                        $ref: Maybe Text,
                        type: Maybe Text
                    })
                })
            }),
            commands: List {
                name: Text,
                description: Maybe Text,
                experimental: Maybe Bool,
                parameters: Maybe (List {
                    name: Text,
                    description: Maybe Text,
                    deprecated: Maybe Bool,
                    optional: Maybe Bool,
                    $ref: Maybe Text,
                    type: Maybe Text,
                    items: Maybe ({
                        $ref: Maybe Text,
                        type: Maybe Text
                    })
                }),
                returns: Maybe (List {
                    name: Text,
                    description: Maybe Text,
                    deprecated: Maybe Bool,
                    optional: Maybe Bool,
                    $ref: Maybe Text,
                    type: Maybe Text,
                    items: Maybe ({
                        $ref: Maybe Text,
                        type: Maybe Text
                    })
                })
            },
            events: Maybe (List {
                name: Text,
                description: Maybe Text,
                parameters: Maybe (List {
                    name: Text,
                    description: Maybe Text,
                    type: Maybe Text,
                    $ref: Maybe Text,
                })
            })
        }
    }
|]
