## Creating New LTV Decomposition

This guide describes the process of creating new LTV breakdown.


LTV can be calculated and broken down across groups such as acquisition channel, cohort and download diet.

## Create Breakdown Input

The first step is to create the breakdown table in the `analysts` schema, where each user is mapped to the desired group.

The user included in the table will be the target group, so the set of users should be filtered accordingly. For example, if you want to calculate LTV for each of the various family of paid search acquisition, then you should only include users who were acquired through `Paid Search`.

The following table is an example :

sso_user_id | paid_search_geo | paid_search_family
----------- | --------------- | ------------------
djklsjdnafl | US | pp
asdnvjdapsv | US | pp
ghriod973kl | 1 | pp
ghriod973kl | 2 | pp
fnkflasdvsd | US | cv
... | ... | ...
... | ... | ...

This input will result in an LTV breakdown for each unique combination of `paid_search_geo` and `paid_search_family`.


paid_search_geo | paid_search_family | LTV
--------------- | ------------------ | ---
US | pp | 100
1 | pp | 200
2 | pp | 150
US | cv | 130


The table needs to pass the following conditions:

* A table name following the [Redshift Best Practice Convention](https://docs.google.com/document/d/1P3UaQyaBBzKL-QaOX4MpUceQnSY-uq5BWc6V6uIzYsw/edit) but with the team name appended e.g. `customer_rpt_element_paid_search_breakdown`
* Create a Pull Request and requesting one from each DS and DP to review it.
* The table should have the `sso_user_id`.
* The table should not contain any other variables other than the desired breakdown.
* Each user can only belong to a single group.

## Output and Review

Once the PR has been approved, the table will be created. We will then generate the LTV and save it back to the `analysts` schema with the name `data_science_<input_table_name>`.

The output will first be created in staging to allow analysts to compare the changes before committing into production.