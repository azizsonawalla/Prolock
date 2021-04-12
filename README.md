# Prolock
Local encrypted vault built with Prolog.

## Usage

### Launch the Program

From root, execute `./prolock.sh` or `swipl src/prolock.pl`

If this is your first time using, you should see the welcome screen.

Create a master password for your vault, which will be used to encrypt/decrypt your data:
```
█████╗   ██████╗   ██████╗  ██╗       ██████╗   ██████╗ ██╗  ██╗
██╔══██╗ ██╔══██╗ ██╔═══██╗ ██║      ██╔═══██╗ ██╔════╝ ██║ ██╔╝
██████╔╝ ██████╔╝ ██║   ██║ ██║      ██║   ██║ ██║      █████╔╝ 
██╔═══╝  ██╔══██╗ ██║   ██║ ██║      ██║   ██║ ██║      ██╔═██╗ 
██║      ██║  ██║ ╚██████╔╝ ███████╗ ╚██████╔╝ ╚██████╗ ██║  ██╗
╚═╝      ╚═╝  ╚═╝  ╚═════╝  ╚══════╝  ╚═════╝   ╚═════╝ ╚═╝  ╚═╝

Welcome new user!
Create a new vault to get started.


Set new password for vault.
You may use letters, numbers, and ~!@#$^&*.

> New vault password: 312ubc123
> Re-enter password: 312ubc123
```

### Create a vault

Type in any password, for example: `312ubc123`, and hit enter.

You should see the menu:
```
What would you like to do?

        1: Add a new username/password entry.
        2: Search for a username/password in the vault.
        3: Delete an entry from the vault.
        4: Exit (vault will be automatically locked).

> Enter number: 
```

### How to add a record

Type `1` and hit enter.

You will be prompted to create a Record with a domain name, username, and password. 

(Note: Each username can have one password for the domain; but the same username can have different passwords across different domains.)

```
> Enter number: 1

Enter details for the new credentials...

> Domain name: www.google.com
> Username: amy@gmail.com
> Password: amy1998

==============================================

Success! Added <zack@gmail.com, zack123!_> to www.google.com

==============================================
```

### Showing all records in Vault

Enter `2` and then `1` to show the records in the entire Vault. The records in each domain are organized together.

```
What would you like to do?

        1: Add a new username/password entry.
        2: Search for a username/password in the vault.
        3: Delete an entry from the vault.
        4: Exit (vault will be automatically locked).

> Enter number: 2

Enter details for record to lookup...

Select a search scope...

        1: Show entire vault.
        2: Look up domain in vault.
        
> Enter number: 1 

==============================================

Results from Vault:

|www.google.com: 
|--amy@gmail.com: amy1998
|--zack@gmail.com: agooglepassword
|--dylan@google.com: dylanlovesdogs
|www.quora.com: 
|--amy: amy1998
|--zack123: aquorapassword

==============================================
```

### Getting records from a specific domain

Enter `2` and then type your domain. You have the option to either show all the records or just a single record from that domain.

* Show all records for domain

```
What would you like to do?

        1: Add a new username/password entry.
        2: Search for a username/password in the vault.
        3: Delete an entry from the vault.
        4: Exit (vault will be automatically locked).

> Enter number: 2

Enter details for record to lookup...

Select a search scope...

        1: Show entire vault.
        2: Look up domain in vault.

> Enter number: 2
> Domain name: www.google.com

Select a search scope...

        1: Show entire domain.
        2: Look for record in domain.

> Enter number: 1
 
==============================================

Results from Vault:

|www.google.com: 
|--amy@gmail.com: amy1998
|--zack@gmail.com: agooglepassword
|--dylan@google.com: dylanlovesdogs

==============================================
```

* Show a single record

Simply specify the corresponding username to query the record.

```
What would you like to do?

        1: Add a new username/password entry.
        2: Search for a username/password in the vault.
        3: Delete an entry from the vault.
        4: Exit (vault will be automatically locked).

> Enter number: 2

Enter details for record to lookup...

Select a search scope...

        1: Show entire vault.
        2: Look up domain in vault.

> Enter number: 2 
> Domain name: www.google.com

Select a search scope...

        1: Show entire domain.
        2: Look for record in domain.

> Enter number: 2
> Username: zack@google.com

```


