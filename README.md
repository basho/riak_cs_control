# What is Riak CS Control?

Riak CS Control is a standalone user management application for Riak CS.

## Configuring

In the ```etc/app.config``` file, configure the application with the information needed to connect to the Riak CS cluster you which to administer.

```
    {riak_cs_control, [
       %% What port to run the application on.
       {port, 8000 },

       %% Instance of Riak CS you wish to talk to.
       {cs_hostname, "s3.amazonaws.com" },
       {cs_port, 80 },
       {cs_protocol, "http" },

       %% Proxy information; necessary if you are using s3.amazonaws.com as
       %% your hostname.
       {cs_proxy_host, "localhost" },
       {cs_proxy_port, 8080 },

       %% Credentials you want the application to run as.
       {cs_admin_key, "admin-key" },
       {cs_admin_secret, "admin-secret" },

       %% Specify the bucket name for administration options.
       {cs_administration_bucket, "riak-cs" }
    ]},
```

## Running 

Start Riak CS Control as you would Riak or Riak CS with the following: ```riak-cs-control start```