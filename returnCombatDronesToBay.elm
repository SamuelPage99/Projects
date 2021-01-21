returnCombatDronesToBay : ReadingFromGameClient -> Maybe DecisionPathNode
returnCombatDronesToBay readingFromGameClient =
    readingFromGameClient.dronesWindow
        |> Maybe.andThen
            (\dronesWindow ->
                case ( dronesWindow.droneGroupInBay, dronesWindow.droneGroupInLocalSpace ) of
                    ( Just droneGroupInBay, Just droneGroupInLocalSpace ) ->
                        let
                            dronesInLocalSpaceQuantity =
                                droneGroupInLocalSpace.header.quantityFromTitle |> Maybe.withDefault 0

                            droneGroupExpectedDisplayText =
                                "Hobgoblin (4)"
                        in
                        if dronesInLocalSpaceQuantity > 0 then
                            Just
                                (describeBranch "Return mining drones"
                                    (case getDescendantWithDisplayText droneGroupExpectedDisplayText dronesWindow.uiNode of
                                        Nothing ->
                                            describeBranch "Could not return mining drones. Trying to return all drones."
                                                (useContextMenuCascade
                                                    ( "drones group", droneGroupInLocalSpace.header.uiNode )
                                                    (useMenuEntryWithTextContaining "Return to drone bay" menuCascadeCompleted)
                                                    readingFromGameClient
                                                )

                                        Just droneGroupUiNode ->
                                            useContextMenuCascade
                                                ( "drones group", droneGroupUiNode )
                                                (useMenuEntryWithTextContaining "Return to drone bay (4)" menuCascadeCompleted)
                                                readingFromGameClient
                                    )
                                )

                        else
                            Nothing

                    _ ->
                        Nothing
            )
