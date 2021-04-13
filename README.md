# :lock: ProLock

Local encrypted vault built entirely with Prolog.

:heavy_check_mark: AES-256 (military grade) encryption for vault data

:heavy_check_mark: SHA-256 irreversable password hashing

:heavy_check_mark: Randomized nonce salted encryption, with 1000 iterations

![Prolock_gif](https://user-images.githubusercontent.com/35513492/114596081-2a452c00-9c5d-11eb-9e18-d0bd5e930619.gif)

## Requirements

* SWI-Prolog >= 7.6.4
* OpenSSL >= 1.1.1f  (31 Mar 2020)
* Linux, MacOS or Windows WSL2

## Quickstart

### Clone the repository

```
git clone git@github.com:azizsonawalla/Prolock.git
```

### Launch the Program

From root, execute 

```
:Prolock$ ./prolock.sh
```
 or 
 
 ```
:Prolock$ swipl src/prolock.pl
```

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

> New vault password: <your master password>
> Re-enter password: <your master password>
```

:exclamation: Make sure you set a strong password and do not forget it! The vault has no recovery mechanism.

### Perform vault actions

After creating a new vault you should see the following main menu every time you login:

```
What would you like to do?

        1: Add a new username/password entry.
        2: Search for a username/password in the vault.
        3: Delete an entry from the vault.
        4: Exit (vault will be automatically locked).

> Enter number: 
```

Choose an action by entering a value between 1 and 4.

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
> Username: zack@gmail.com
 
==============================================

Results from Vault:

|www.google.com: 
|--zack@gmail.com: agooglepassword

==============================================
```

### Deleting records

You can delete either a single record, or all records in one domain.

* Delete a single record
```
What would you like to do?

        1: Add a new username/password entry.
        2: Search for a username/password in the vault.
        3: Delete an entry from the vault.
        4: Exit (vault will be automatically locked).

> Enter number: 3
What would you like to delete?

        1: Delete username/password.
        2: Delete entire domain.

> Enter number: 1

Enter details for credential to delete...
> Domain name: www.google.com
> Username: zack@gmail.com

==============================================

Success! Deleted <zack@gmail.com> from www.google.com

==============================================
```

* Delete all records in a domain
```
What would you like to do?

        1: Add a new username/password entry.
        2: Search for a username/password in the vault.
        3: Delete an entry from the vault.
        4: Exit (vault will be automatically locked).

> Enter number: 3
What would you like to delete?

        1: Delete username/password.
        2: Delete entire domain.

> Enter number: 2

Enter details for domain to delete...
> Domain name: www.quora.com

==============================================

Success! Deleted www.quora.com

==============================================
```

Using the above actions, the vault will have the following records remaining:
```
==============================================

Results from Vault:

|www.google.com: 
|--amy@gmail.com: amy1998
|--dylan@google.com: dylanlovesdogs

==============================================
```

### Exiting and locking the vault

Once all operations are done, you can exit the vault by typing `4`. The data will be encrypted and saved to your disk.
```
What would you like to do?

        1: Add a new username/password entry.
        2: Search for a username/password in the vault.
        3: Delete an entry from the vault.
        4: Exit (vault will be automatically locked).

> Enter number: 4

Locking the vault. Please wait...

                                                 
     ___   ___   ___   ___   ___   ___           
    |   | |   | |   | |   | |   | |   |          
    | 1 | | 0 | | 2 | | 5 | | 8 | | 0 |
    |___| |___| |___| |___| |___| |___|     
    
```

If the data was saved successfully, you will see the following message:

```
                                           ████  
     ___   ___   ___   ___   ___   ___    █    █ 
    |   | |   | |   | |   | |   | |   |  ████████
    | L | | O | | C | | K | | E | | D |  █  ||  █
    |___| |___| |___| |___| |___| |___|  ████████
```

## Development

To run the tests, use the following command:
```swipl test/TEST_FILE.pl```

For example:
```swipl test/dictionary_test.pl```

## Disclaimer

This software is distributed with no guarantees whatsoever. Use at your own risk.
