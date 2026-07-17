#pragma once
#include <string_view>

double smallian(double diameterLarge, double diameterSmall, double logLength);

double scribner(double DIA, double LEN, char COR);

double intl14(double DIB, double LENGTH);

double treeFormClass78BoardFootTable(double dbh, double numberOf16FootLogs, std::string_view boardFootTable);

double treeFormClass78BoardFootFormula(double dbh, double numberOf16FootLogs, std::string_view boardFootTable);

double biaBehBoardfoot(double dia, double loglength);