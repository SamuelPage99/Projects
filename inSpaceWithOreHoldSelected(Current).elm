module inSpaceWithOreHoldSelected%28Current%29 exposing (..)inSpaceWithOreHoldSelected : BotDecisionContext -> SeeUndockingComplete -> EveOnline.ParseUserInterface.InventoryWindow -> DecisionPathNode
inSpaceWithOreHoldSelected context seeUndockingComplete inventoryWindowWithOreHoldSelected =
    if seeUndockingComplete.shipUI |> shipUIIndicatesShipIsWarpingOrJumping then
        describeBranch "I see we are warping."
            ([ returnDronesToBay context.readingFromGameClient
             , readShipUIModuleButtonTooltips context
             ]
                |> List.filterMap identity
                |> List.head
                |> Maybe.withDefault waitForProgressInGame
            )

    else
        case context |> knownModulesToActivateAlways |> List.filter (Tuple.second >> .isActive >> Maybe.withDefault False >> not) |> List.head of
            Just ( inactiveModuleMatchingText, inactiveModule ) ->
                describeBranch ("I see inactive module '" ++ inactiveModuleMatchingText ++ "' to activate always. Activate it.")
                    (clickModuleButtonButWaitIfClickedInPreviousStep context inactiveModule)

            Nothing ->
                case inventoryWindowWithOreHoldSelected |> capacityGaugeUsedPercent of
                    Nothing ->
                        describeBranch "I do not see the ore hold capacity gauge." askForHelpToGetUnstuck

                    Just fillPercent ->
                        let
                            describeThresholdToUnload =
                                (context.eventContext.appSettings.oreHoldMaxPercent |> String.fromInt) ++ "%"
                        in
                        if context.eventContext.appSettings.oreHoldMaxPercent <= fillPercent then
                            describeBranch ("The ore hold is filled at least " ++ describeThresholdToUnload ++ ". Unload the ore.")
                                (returnDronesToBay context.readingFromGameClient
                                    |> Maybe.withDefault (dockToUnloadOre context)
                                )

                        else
                            describeBranch ("The ore hold is not yet filled " ++ describeThresholdToUnload ++ ". Get more ore.")
                                (case context.readingFromGameClient.targets |> List.head of
                                    Nothing ->
                                        describeBranch "I see no locked target."
                                            (travelToMiningSiteAndLaunchDronesAndTargetAsteroid context)

                                    Just _ ->
                                        {- Depending on the UI configuration, the game client might automatically target rats.
                                           To avoid these targets interfering with mining, unlock them here.
                                        -}
                                        unlockTargetsNotForMining context
                                            |> Maybe.withDefault
                                                (describeBranch "I see a locked target."
                                                    (case context |> knownMiningModules |> List.filter (.isActive >> Maybe.withDefault False >> not) |> List.head of
                                                        Nothing ->
                                                            describeBranch "All known mining modules are active."
                                                                (readShipUIModuleButtonTooltips context
                                                                    |> Maybe.withDefault waitForProgressInGame
                                                                )

                                                        Just inactiveModule ->
                                                            describeBranch "I see an inactive mining module. Activate it."
                                                                (clickModuleButtonButWaitIfClickedInPreviousStep context inactiveModule)
                                                    )
                                                )
                                )