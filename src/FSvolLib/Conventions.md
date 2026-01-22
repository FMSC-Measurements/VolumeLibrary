## Data Type Conventions
### Floating Point Numbers
For all floating point numbers use double precission for final results for calculations.
When preforming calculations: prefer using numbers in units that minimize the need for decimals in calculations
For example if the lowest presision for a calculation is in 1/100ths of an inch and the initial input for a
calculation is 18.11 inches convert the unit for the calculation to 1811 1/100ths before preforming calculations
then at the end of the calculation convert the final result back to inches by deviding 100 or whatever is approriate.

## Naming Conventions
### Classes/Types
Use pascal case for all classes and types. e.g. `MyClassName'

### Fields and Variables
Use camel case. e.g. `myFiedName`.
When Nameing class/struct fields prefer names that minimize ambiguity. As well minimize abreviations and shortening of words

### Abreviations, Acronyms and Initialisms
When a abreviation, acronyms or initialism is used in the name of a field or class, use lower case for letters except the first unless using camelcase and it is the first letter of the field name. e.x. `XmlElement`, `fiaCode`

#### Class Internal Fields
To help deferintiate variables that are scoped to a class, from local/function variables use a trailing underscore on field names.
e.x. `myClassFieldName_`

## Plane old Data Objects (PODO)
Plane old data objects are types that are use primarly for storing/transfering data. 
They do not contain any logic/functions and are typicly struct types. 
When defining a PODO only create the header file for it as it shouldn't contain any logic. 