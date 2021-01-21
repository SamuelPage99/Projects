-- I would like to modify this to let me click on specific drones group to return to bay.

returnDronesToBay : ReadingFromGameClient -> Maybe DecisionPathNode
returnDronesToBay readingFromGameClient =
    readingFromGameClient.dronesWindow
        |> Maybe.andThen .droneGroupInLocalSpace
        |> Maybe.andThen
            (\droneGroupInLocalSpace ->
                if (droneGroupInLocalSpace.header.quantityFromTitle |> Maybe.withDefault 0) < 1 then
                    Nothing

                else
                    Just
                        (describeBranch "I see there are drones in local space. Return those to bay."
                            (useContextMenuCascade
                                ( "drones group", droneGroupInLocalSpace.header.uiNode )
                                (useMenuEntryWithTextContaining "Return to drone bay" menuCascadeCompleted)
                                readingFromGameClient
                            )
                        )
            )
