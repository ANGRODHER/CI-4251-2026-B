# Test 3

The following test will have just a single question. Please read the specification carefully and answer it in the `Sol.hs` file.

You CAN (and should) use any library that's **available** in the current project, but you should not add any new dependency.
If you add a new dependency you WILL get a 0 on the test.

This test will not be done in groups. That is, you must work alone.

## PokeQuery

This test will quiz you on your knowledge of optics.

We will use the [PokeSet](https://raw.githubusercontent.com/Biuni/PokemonGO-Pokedex/master/pokedex.json) dataset for every single question.

### Part 1

Model the `PokeSet` dataset as a haskell data type. You must explain
why did you choose that particular representation, and what are the advantages and disadvantages of it.

```haskell
data PokeSet optionalTypeArgument1 optionalTypeArgument2 ...
```

### Part 2

Implement a `pPokeSet` function which parses the data set to each representation provided in Part 1.

### Part 3

Implement the following queries using `optics`, explain your reasoning
for each optic used (e.g. why did you choose a `Traversal` instead of a `Fold`?).
You MUST use optics only: no let-bindings, where clauses or any other way of introducing intermidiate
variables.
You MAY declare auxiliary types and optics if you want to.

```haskell
-- Returns a with the names of each pokemon in the dataset.
pokeNames :: [String]
pokeNames = undefined

-- Returns a list with the names of each pokemon and its next evolutions
-- That is: ("Bulbasaur", ["Ivysaur", "Venusaur"]) and ("IviSaur", ["Venusaur"])
-- are both in the list
pokeEvolutions :: [(String, [String])]
pokeEvolutions = undefined

-- Same as pokeEvolutions, but it should return only the base pokemons
-- (that is, the pokemons that are not evolutions of any other pokemon).
-- Do NOT use or mention `pokeEvolutions` in your implementation.
pokeEvolutions' :: [(String, [String])]
pokeEvolutions' = undefined

-- Filters all the pokemons that are of type "Psychic" and "Normal"
-- increasing their multipliers by 2.
pokePsychicNormal :: PokeSet SomeTypeArgument1
pokePsychicNormal = undefined

-- Filters all the pokemons that are of type "Psychic" or "Normal"
-- decreasing their multipliers by 1.
pokePsychicNormal' :: PokeSet SomeTypeArgument1
pokePsychicNormal' = undefined

-- set the image of the pokemons `x` that have an an evolution `y`,
-- such that their evolution `y` weights more than them to the image of `y`.
pokeDrinker :: PokeSet SomeTypeArgument1
pokeDrinker = undefined


-- Return the name(s) of the pokemon(s) with the most amount of weaknesses.
poleWeakest :: [String]
pokeWeakest = undefined

-- Returns the average weight of all the pokemons in the dataset.
pokeAvgWeight :: Double
pokeAvgWeight = undefined

-- Returns the variance of the weight of all the pokemons in the dataset.
pokeVarWeight :: Double
pokeVarWeight = undefined

-- Returns the pearson correlation coefficient between the weight and height of all the pokemons in the dataset.
pokeCorr :: Double
pokeCorr = undefined

-- Modifies every `"name": pokeName` field in the dataset to `"name": pokeName tuff`,
-- where `pokeName` is the original name of the pokemon, and `pokeName tuff` is the original name concatenated with the string " tuff".
-- If the name already ends with "tuff", it should not be modified.
-- Remember that name can appear in multiple places in a single record.
pokeTuff :: PokeSet SomeTypeArgument1
pokeTuff = undefined

-- Returns the Quantile 1,2,3 and the Interquantile range of the SPAWN TIME of all the pokemons in the
-- dataset.
pokeIQR :: (Double,Double,Double,Double)
pokeIQR = undefined

-- Use the pokeIQR to build a visual representation of the box plot.
pokeBoxPlot :: String
pokeBoxPlot = undefined

-- Returns a contingency table of the types (rows)/weaknesses (cols) of the pokemons in the dataset.
-- The representation of the contingency table is up to you, but it MUST be indexable
-- using a tuple: `pokeContingency ^? ix ("water","fire")`
pokeContingency = undefined

-- Build an histogram for the egg distance. It's up to you to decide the
-- intervals.
-- The histogram should have a `Show` instance that pretty prints it.
-- The `Show` instance should not hold any logic, it should only pretty print.
pokeHist :: Histo
pokeHist = undefined
```
