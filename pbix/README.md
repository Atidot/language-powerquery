# pbix
PowerBI `.pbix` file coder/decoder


# [Build]

# CLI
### Print a formula (M script) from DataMashup/Formulas
~~~ shell
12:55 barak@berkos:~/Development/atidot/language-powerquery/build (master) $ ./result/bin/pbix print --path ../examples/PowerBI_File_PBIX/iris.pbix --formula Section1.m
section Section1;

shared iris = let
    Source = Csv.Document(File.Contents("\\VBOXSVR\Shared\PowerBI\iris.csv"),[Delimiter=",", Columns=5, Encoding=1252, QuoteStyle=QuoteStyle.None]),
    #"Promoted Headers" = Table.PromoteHeaders(Source, [PromoteAllScalars=true]),
    #"Changed Type" = Table.TransformColumnTypes(#"Promoted Headers",{{"sepal_length", type number}, {"sepal_width", type number}, {"petal_length", type number}, {"petal_width", type number}, {"species", type text}})
in
    #"Changed Type";
12:55 barak@berkos:~/Development/atidot/language-powerquery/build (master) $
~~~

### Print the Haskell AST (Abstract Syntax Tree) of a formula
~~~ shell
12:56 barak@berkos:~/Development/atidot/language-powerquery/build (master) $ ./result/bin/pbix print --path ../examples/PowerBI_File_PBIX/iris.pbix --formula Section1.m -x | head -c 400
SectionDocument (Section {_section_attributes = Nothing, _section_name = Just (RegularI "Section1"), _section_members = Just [SectionMember {_sectionMember_attributes = Nothing, _sectionMember_shared = True, _sectionMember_name = Reg
ularI "iris", _sectionMember_expression = LetE (LetExpression {_letExpression_variableList = [Variable {_variable_name = RegularI "Source", _variable_expression = Logi
12:57 barak@berkos:~/Development/atidot/language-powerquery/build
~~~

### Print the AST as a JSON
- ([iris dataset])
~~~ shell
12:58 barak@berkos:~/Development/atidot/language-powerquery/build (master) $ ./result/bin/pbix print --path ../examples/PowerBI_File_PBIX/iris.pbix --formula Section1.m -x -j | head -c 1000
{"tag":"SectionDocument","contents":{"_section_attributes":null,"_section_name":{"tag":"RegularI","contents":"Section1"},"_section_members":[{"_sectionMember_expression":{"tag":"LetE","contents":{"_letExpression_expression":{"tag":"L
ogicalE","contents":{"tag":"And_OE","contents":{"tag":"Is_LAE","contents":{"tag":"As_IE","contents":{"tag":"EqualityAE","contents":{"tag":"RelationalEE","contents":{"tag":"AdditiveRE","contents":{"tag":"MultiplicativeAE","contents":{
"tag":"MetadataME","contents":{"_metadataExpression_second":null,"_metadataExpression_annotation":[],"_metadataExpression_first":{"tag":"UnaryType","contents":{"tag":"Primary_TE","contents":{"tag":"FieldAccessPE","contents":{"_implic
itTargetProjection_annotation":[],"tag":"ImplicitTargetProjection","_implicitTargetProjection_identifier":{"tag":"QuotedI","contents":"#\"Changed Type\""}}}}}}}}}}}}}}},"_letExpression_variableList":[{"_variable_annotation":[],"_vari
able_expression":{"tag":"LogicalE","contents":{"tag":"And_OE","conte
12:58 barak@berkos:~/Development/atidot/language-powerquery/build (master) $
~~~

### Print all String Literals (using [jq])
~~~ shell
12:25 barak@berkos:~/Development/atidot/language-powerquery/build (master) $ ./result/bin/pbix print --path ../examples/PowerBI_File_PBIX/iris.pbix --formula Section1.m -x -j | jq -c 'paths as $path | select(getpath($path) == "String
L") | getpath($path[:-1]) | .contents'
"\"\\\\VBOXSVR\\Shared\\PowerBI\\iris.csv\""
"\",\""
"\"sepal_length\""
"\"sepal_width\""
"\"petal_length\""
"\"petal_width\""
"\"species\""
~~~

### Print all Variables names (using [jq])
~~~ shell
12:47 barak@berkos:~/Development/atidot/language-powerquery/build (master) $ ./result/bin/pbix print --path ../examples/PowerBI_File_PBIX/iris.pbix --formula Section1.m -x -j | jq  'paths as $path | select($path[-1] == "_variable_name") | getpath($path) | .contents'
"Source"
"#\"Promoted Headers\""
"#\"Changed Type\""
~~~

[jq]: https://stedolan.github.io/jq/
[Build]: https://github.com/Atidot/language-powerquery#Build
[iris dataset]: https://www.kaggle.com/arshid/iris-flower-dataset
