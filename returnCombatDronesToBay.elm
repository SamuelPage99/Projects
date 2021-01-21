-- Here is a working launchAndEngageCombatDrone. I would like to know how to do the same customization but with the returnDrones function.

launchAndEngageCombatDrones : ReadingFromGameClient -> Maybe DecisionPathNode
launchAndEngageCombatDrones readingFromGameClient =
    readingFromGameClient.dronesWindow
        |> Maybe.andThen
            (\dronesWindow ->
                case ( dronesWindow.droneGroupInBay, dronesWindow.droneGroupInLocalSpace ) of
                    ( Just droneGroupInBay, Just droneGroupInLocalSpace ) ->
                        let
                            idlingDrones =
                                droneGroupInLocalSpace.drones
                                    |> List.filter (.uiNode >> .uiNode >> EveOnline.ParseUserInterface.getAllContainedDisplayTexts >> List.any (String.toLower >> String.contains "idle"))

                            dronesInBayQuantity =
                                droneGroupInBay.header.quantityFromTitle |> Maybe.withDefault 0

                            dronesInLocalSpaceQuantity =
                                droneGroupInLocalSpace.header.quantityFromTitle |> Maybe.withDefault 0

                            droneGroupExpectedDisplayText =
                                "Hobgoblin (4)"
                        in
                        if 0 < (idlingDrones |> List.length) then
                            Just
                                (describeBranch "Engage idling drone(s)"
                                    (useContextMenuCascade
                                        ( "drones group", droneGroupInLocalSpace.header.uiNode )
                                        (useMenuEntryWithTextContaining "engage" menuCascadeCompleted)
                                        readingFromGameClient
                                    )
                                )

                        else if 0 < dronesInBayQuantity && dronesInLocalSpaceQuantity < 5 then
                            Just
                                (describeBranch "Launch drones"
                                    (case getDescendantWithDisplayText droneGroupExpectedDisplayText dronesWindow.uiNode of
                                        Nothing ->
                                            describeBranch
                                                ("Did not find the node with display text '" ++ droneGroupExpectedDisplayText ++ "'")
                                                askForHelpToGetUnstuck

                                        Just droneGroupUiNode ->
                                            useContextMenuCascade
                                                ( "drones group", droneGroupUiNode )
                                                (useMenuEntryWithTextContaining "Launch drones (4)" menuCascadeCompleted)
                                                readingFromGameClient
                                    )
                                )

                        else
                            Nothing

                    _ ->
                        Nothing
            )
