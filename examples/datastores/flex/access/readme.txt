This demo shows how a TVpFlexDatastore can link to an Access database using 
ODBC.


Step-by-step instructions for creating a new application
--------------------------------------------------------

- If a suitable Access database is not available from somewhere else
  create an empty Access database containing the VisualPlanit tables and fields.
  The easiest way to do this is by means of the CreateAccessDB application in 
  the folder "tools". Use extension .mdb to create an old Access 97/2000 file,
  or use extension .accdb for a new Access 2007+ file.
    
- Create a new form.

- Add a TVpFlexDatastore, a TVpControlLink and the visual planner components  
  needed.
  
- Add an TODBConnection. Add a TSQLTransationSet

- Set these properties of the ODBCConnection:
  - "Driver" ---> "Microsoft Access Driver (*.mdb, *.accdb)"
  - "Params" ---> Add "DBQ=full/path/to/access/file"
    Note: Make sure to use the full path in Params, otherwise the fieldmapper
    in one of the next steps will not find the database.
  - "Transaction --> instance of the SQLTransaction.
    
- Add four TSQLQuery components and set these properties
  - "Database" ---> instance of the ODBCConnection
  - "Options" --> add sqoAutoApplyUpdates, sqoAutoCommit and sqoKeepOpenOnCommit
  - "UsePrimaryKeyAsKey" --> false (VERY IMPORTANT FOR ACCESS FILES!)
  
- Enter these SQL for the queries where <tablename> must be replaced by 
  Resources, Contacts, Events and Tasks.

  SELECT * FROM <tablename>
  
- For the Resources query add this UpdateSQL:

  UPDATE
    Resources
  SET
    Description = :Description,
    Notes = :Notes,
    ResourceActive = :ResourceActive,
    ImageIndex = :ImageIndex
  WHERE
    ResourceID = :ResourceID   
  
- Similarly, add UpdateSQL instructions for the Events, Contacts, and Tasks tables.
  Make sure to include all database fields (except for the AutoInc fields).
  See the demo application.
  
- Add this text to the DeleteSQL property of the Resources SQLDataset

  DELETE * FROM Resources
  WHERE ResourceID = :ResourceID.
  
- This is the DeleteSQL needed for the Events dataset:

  DELETE * FROM Events
  WHERE RecordID = :RecordID   
  
- Repeat accordingly with the Contacts and Rasks datasets.

- Add four TDatasource components. Link each one of them to a TSQLDataset.

- Link the datasource components to the matching Datasources of the 
  VpFlexDatastore.
  
- In OnCreate of the form set 
    VpFlexDatastore.Connected := true
    ODBConnection.Connected := true
    SQLTransaction.Active := true

- In OnDestroy set 
    ODBCConnection.Connected := false
    
- Double-click on the VpFlexDatastore in Form1 to open the field mapper.

- Select "Resources" in the combobox. Click a database field in the left listbox
  and click its corresponding planner field in the right listbox. Click add to
  store this mapping. Repeat with all database fields.
  
- Or, if all database fields have the same name as the corresponding planner 
  fields, simply click "Add all" to establish a mapping for all equally names
  fields. 
  
- Note: the sample database does not support the User-defined fields of the 
  planner. Therefore, these fields will be left without a matching database
  field.
  
- Repeat with the Events, Contacts and Tasks tables. 

- Close the field mapper.

- After the field mappings have been established the absolute database path can
  be removed from the Params of the ODBCConnection and replaced by a relative
  path or can be set at runtime.

- Done.


  
