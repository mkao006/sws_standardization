


readInputFile = function(treeData, extractionRateData, inputData,
    year, suaData){
    tree = data.table(
        read.csv(
            file = treeData,
            stringsAsFactors = FALSE
            )
        )
    extractionRate =
        data.table(
            read.csv(
                file = extractionRateData,
                stringsAsFactors = FALSE
                )
            )
    extractionRate[(Num == 0 & Symb == "C") | Symb == "M", Num := NA]
    ## This is temporary before we move on multiple years
    extractionRate = extractionRate[Year == year, ]


    
    ## Construct Node Table
    ## --------------------
    ##
    ## Read the nodes table from the default network which take only
    ## the item code, default extraction rate and whether it is a
    ## target or not. The default rate is modified by the new country
    ## specific extraction rate from SUA.
    ##
    ## Variables:
    ## * Item.Code
    ## * Item.Name (Maybe this should be taken from the SUA.)
    ## * The final FBS codes.
    ## * Default.Extraction.Rate
    ## * Aggregate.Method
    ## * Weight
    ## * Target (T, B, F etc...)
    ## * Use.Calorie
    ## * Specfic rates from SUA.


    nodes.dt =
        merge(
            tree[, list(Item.Code, Item.Name, Default.Extraction.Rates,
                        Aggregate.Method, Weight, Target, Use.Calorie
                        )
                 ],
            extractionRate[, list(Item.Code, Num)],
            by = "Item.Code",
            all = TRUE
            )

    nodes.dt[,
             finalExtractionRate :=
             ifelse(Aggregate.Method %in% c("T", "X") |
                    (Aggregate.Method == "B" & Weight == 0), 10000,
                    ifelse(!is.na(Num), Num,
                           ifelse(!is.na(Default.Extraction.Rates),
                                  Default.Extraction.Rates, NA
                                  )
                           )
                    )
             ]

    nodes.dt[, Default.Extraction.Rates := NULL]
    nodes.dt[, Num := NULL]
    
    targets =
        nodes.dt[Target == "Target" |
                 (Aggregate.Method == "B" & Weight == 0),
                 Item.Code]
    ## Construct Edge Table
    ## --------------------
    ##
    ## This will consist of two components, the first will be the
    ## default edges from the default network. Then the input file
    ## will modify the edges. The share will also contain information
    ## such as shares.
    ##
    ## Variables:
    ## * Child
    ## * Parent
    ## * Shares (reciprocal calculated from input)

    input =
        data.table(
            read.csv(file = inputData,
                     stringsAsFactors = FALSE
                     )
            )
    input =
        input[!(Num == 0) &
              !Child.Item.Code %in% targets &
              Year == year,
              list(Child.Item.Code, Item.Code, Num)]
    input[, reciprocalShare := Num/sum(Num, na.rm = TRUE),
          by = "Child.Item.Code"]
    input[, Num := NULL]
    setnames(input,
             old = c("Child.Item.Code", "Item.Code"),
             new = c("child", "parent"))


    ## initEdge.dt = tree[, list(Item.Code, FBS.Parent.Code)]
    ## setnames(initEdge.dt, c("Item.Code", "FBS.Parent.Code"),
    ##          c("child", "parent"))
    ## Standardize to Targets instead
    initEdge.dt = tree[, list(Item.Code, Target.Parent.Code)]
    setnames(initEdge.dt, c("Item.Code", "Target.Parent.Code"),
             c("child", "parent"))
    initEdge.dt[, reciprocalShare := as.numeric(NA)]


    ## print(str(initEdge.dt))
    ## print(input)

    edgeUnion.dt = rbind(initEdge.dt[!child %in% input$child, ], input)


    edges.dt =
        edgeUnion.dt[, reciprocalShare := sumWithNA(reciprocalShare),
                     by = c("child", "parent")]

    
    edges.dt[is.na(reciprocalShare), reciprocalShare := as.numeric(1)]

    ## Construct the standardization table
    ## -----------------------------------
    ##
    ## This table contains the final rate between the item and its
    ## immediate parent after accounting the shares, extraction rate
    ## and conversion ratio.
    ##
    ## Variables:
    ## * child
    ## * parent
    ## * finalRate
    
    subNodes.dt = nodes.dt[, list(Item.Code, finalExtractionRate)]
    setnames(subNodes.dt, old = "Item.Code", new = "child")
    graph.dt = merge(edges.dt, subNodes.dt, by = "child")

    ## Remove if the parent code is zero, it is a placeholder for no
    ## standardization.
    graph.dt = graph.dt[parent != 0, ]

    ## Remove entry with extraction is zero.
    graph.dt = graph.dt[finalExtractionRate != 0, ]

    ## Compute the final rate to apply
    graph.dt[, finalWeight :=
             reciprocalShare/(finalExtractionRate/10000)]


    graph.dt[, `:=`(c("reciprocalShare", "finalExtractionRate"),
                    NULL)]
    ## Extract final FBS targets
    ## fbsTargets = na.omit(unique(tree$FBS.Code))
    fbsTargets =
        na.omit(unique(tree[Target == "Target" |
                            Aggregate.Method == "X" |
                            Item.Code == Target.Parent.Code,
                            Target.Parent.Code]))

    ## graph.dt[child %in% tree[FBS.Parent.Code == 0, Item.Code],
    ##          finalWeight := 0]
    
    ## print(str(graph.dt))
    
    ## graph.dt =
    ##     rbind(
    ##         graph.dt,
    ##         data.table(child = fbsTargets, parent = fbsTargets,
    ##                    finalWeight = 1
    ##                    )
    ##         )

    
    ## Compute the direct weights
    ## --------------------------
    ##
    ## This is the final table which relate all the item to its final
    ## target in FBS.
    ##
    ## Variables:
    ## * leaves
    ## * root
    ## * directWeight
    
    ## Start standardization
    leave.dt = copy(graph.dt)
    setnames(leave.dt, c("child", "parent", "finalWeight"),
             c("leaves", "inter", "directWeight"))

    root.dt = copy(graph.dt)
    setnames(root.dt, c("parent", "child"),
             c("root", "inter"))


    ## Initiate
    leave.dt = merge(leave.dt, root.dt, by = "inter", all.x = TRUE,
        allow.cartesian = TRUE)
    leave.dt[, directWeight := directWeight * finalWeight]
    leave.dt[, `:=`(c("inter", "finalWeight"), NULL)]
    setnames(leave.dt, "root", "inter")
    iter = 1
    maxIter = 10
    while(!all(leave.dt$inter %in% fbsTargets)){
        if(any(is.na(leave.dt$inter))){
            print("Some leaves are terminated and removed")
            leave.dt = leave.dt[!is.na(inter), ]
        }
        iter = iter + 1
        ## print(head(leave.dt))
        ## print(head(root.dt))
        leave.dt = merge(leave.dt, root.dt, by = "inter", all.x = TRUE,
            allow.cartesian = TRUE)
        leave.dt[, directWeight := directWeight * finalWeight]
        leave.dt[, `:=`(c("inter", "finalWeight"), NULL)]
        if(all(leave.dt$root %in% fbsTargets) | iter == maxIter)
            break
        setnames(leave.dt, "root", "inter")
    }
    ## Check which one has multiple path.

    directWeight.dt =
        leave.dt[, list(directWeight = sumWithNA(directWeight)),
                 by = c("leaves", "root")
                 ]
    setkeyv(directWeight.dt, c("leaves", "root"))
    ## directWeight.dt = directWeight.dt[leaves != root, ]


    ## This simply read the sua data
    sua.dt =
    data.table(
        read.csv(file = suaData,
                 stringsAsFactors = FALSE
                 )
        )
    sua.dt = sua.dt[Year == currentYear, ]
    

    list(nodes = nodes.dt, edge = edges.dt,
         directWeights = directWeight.dt,
         sua = sua.dt)
}

computeStandardization = function(suaData, directWeights, element){
    element.dt = suaData[Element.Code == element, list(Item.Code, Num)]
    setnames(element.dt, c("Item.Code", "Num"), c("leaves", "Value"))  

    standardization.dt =
        merge(element.dt, directWeights, by = "leaves", all = TRUE,
              allow.cartesian = TRUE)

    standardization.dt[, standardizedValue :=
                       Value * directWeight/1000]
    standardization.dt[, sumStandardizedValue :=
                       sum(Value * directWeight, na.rm = TRUE)/1000, by = "root"]
    ## standardized.dt = 
    ##     standardization.dt[!is.na(root),
    ##                        standardizedValue := sum(Value * directWeight, na.rm = TRUE)/1000,
    ##                        by = "root"]
    ## setnames(standardized.dt, c("root", "V1"),
    ##          c("itemCode", "standardizedValue"))
    ## standardized.dt
    standardization.dt
}

    


checkStandardization = function(disseminatedData, standardizedData,
    currentYear, countryCode){
    disseminated =
        disseminatedData[Year == currentYear &
                         FAOST_CODE == countryCode,
                         list(itemCode, name, Value)]
    setnames(disseminated, old = "Value", new = "disseminatedValue")
    disseminated[, itemCode := as.numeric(itemCode)]
    disseminated[, itemName :=
                gsub("Food Balance Sheets_|_Food\\(1000 tonnes\\)", "",
                     name)]
    disseminated[, name := NULL]

    check.dt =
        merge(disseminated, standardizedData,
              by = "itemCode", all.x = TRUE)
    check.dt[, pctDifference :=
             abs(disseminatedValue - round(standardizedValue))/
                 disseminatedValue * 100]
    check.dt[disseminatedValue == 0 & round(standardizedValue) == 0,
             pctDifference := 0]
    check.dt
}

