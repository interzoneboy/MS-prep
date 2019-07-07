# MS-prep
Data formatting and EDA for sieve-based MS, MS/MS data

This project has evolved a lot, and in crazy directions. The magrittr pipeline-based way of doing things has, in the meantime, become the de-facto Best Way Of Doing Things.
This is great, but it does mean that this project should probably be tweaked a bit to bring it more in line, now that it's no longer out in the weeds.

## TODO

* Swap the names of the row-normalize and col-normalize modules and functions therein. They're all backwards! col\_normalize\_to\_whatever actually calculates a normalizer the length of the number of rows, then normalizes each row to it. row\_normalize\_to\_whatever calculates a normalizer the length of the number of columns, and normalized the columns to it. I have no idea why it's named like this -- maybe because in the former case the normalizer *is* a row, and in the latter it *is* a column. However it should be named after what actually gets normalized.
