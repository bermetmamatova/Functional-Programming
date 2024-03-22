module seven
import StdEnv

//bta5bp


/* Create record `City`. It should contain 3 fields: name(String), area(Real)
and population(Int). Write `smallCities` function which takes list of Cities
and returns the names of the cities that have area less than 100 or population less than 300 000.
*/

::City = { name :: String
		, area :: Real
		,population :: Int
		}

budapest={name="Budapest", area=525.0, population=1756000}
kutaisi={name="Kutaisi", area=67.0, population=147000}
debrecen={name="Debrecen", area=461.0, population=202000}
berlin={name="Berlin", area=891.0, population=3645000}
pisa={name="Pisa", area=185.0, population=90000}

smallCities :: [City] -> [String]
smallCities ls = [x.name \\x<-ls| (x.area < 100.0) || (x.population < 300000)]

//Start = smallCities [] // []
//Start = smallCities [budapest,kutaisi,debrecen,berlin,pisa] // ["Kutaisi", "Debrecen", "Pisa"]
//Start = smallCities [budapest,berlin] // []
//Start = smallCities [kutaisi] // ["kutaisi"]