require 'erb'

# Setting for apache
set :subdomain, "myapp"
set :domain, "yoine.org"

set :application, "myapp"

# SSH OPTIONS
ssh_options[:keys] = %w(~/.ssh/id_rsa)
ssh_options[:forward_agent] = true 

# git config
set :repository, "dann@192.168.0.30:/var/git/repos/#{application}"
set :scm, :git
set :scm_username, ENV['svn_user'] || ENV['USER'] || Proc.new { Capistrano::CLI.password_prompt('SCM User: ') }
set :scm_password, Proc.new { Capistrano::CLI.password_prompt('SCM Password: ') }

#for SVN
#set :repository,  "file:///var/svn/repos/#{application}/trunk"
#set :svn_user, ENV['svn_user'] || "dann"
#set :svn_password, Proc.new { Capistrano::CLI.password_prompt('SVN Password: ') }

set :deploy_to, "/u/apps/#{domain}/#{subdomain}/#{application}"

appserver_ips = ["192.168.0.60"]
webserver_ips = ["192.168.0.70"]
dbserver_ip = "192.168.0.50"

# Role definition
role :app, *appserver_ips
role :web, *webserver_ips
role :db,  dbserver_ip, :primary => true

# Web server config
set :apache_web_config_dir, "/etc/httpd/conf.vhosts/"

# App server config
set :apache_app_config_dir, "/etc/httpd/conf.vhosts/"

# DB server config
set :mysql_config_dir, "/etc/mysql"
set :mysql_root_password, Proc.new {Capistrano::CLI.password_prompt('MySQL server ROOT Password: ')}
set :mysql_db_username, "#{application}".downcase
set :mysql_db_password, "password"
set :dbname, "#{application}".downcase

namespace :deploy do
  task :finalize_update, :except => { :no_release => true } do
    stamp = Time.now.utc.strftime("%Y%m%d%H%M.%S")
    asset_paths = %w(images css js).map { |p|
      "#{latest_release}/root/static/#{p}"
    }.join(" ")
    run "find #{asset_paths} -exec touch -t #{stamp} {} ';'; true", :env => { "TZ" => "UTC" }
  end

  # TODO
  task :migrate, :roles => :db, :only => { :primary => true } do
  end

  task :start, :roles => :app do
    sudo "/etc/init.d/httpd start"
  end

  task :stop, :roles => :app do
    sudo "/etc/init.d/httpd stop"
  end

  task :restart do
    restart_apache_web_server
  end

  task :restart_apache_web_server do
    sudo "/etc/init.d/httpd stop"
    sudo "/etc/init.d/httpd start"
  end

  task :setup do
    setup_web_server
    setup_app_server
    setup_database
  end

  task :setup_web_server, :roles => :web do
    create_apache_web_config
    enable_apache_web_config
  end

  task :setup_app_server, :roles => :app do
    create_apache_app_config
    enable_apache_app_config
  end

  task :setup_database, :roles => :db do
    setup_mysql_database
  end

  task :setup_mysql_database, :roles => :db do
    # create_mysql_config
    create_database_and_user
    reload_mysql_server
    # restart_mysql_server
  end

  desc "Restart mysql"
  task :restart_mysql_server, :roles => :db do
    sudo "/etc/init.d/mysqld restart"
  end

  desc "Restart mysql"
  task :reload_mysql_server, :roles => :db do
    sudo "/etc/init.d/mysqld restart"
  end

  desc "Create mysql config"
  task :create_mysql_config, :roles => :db do
    config_template = File.read(File.dirname(__FILE__) + "/templates/mycnf.erb");
    config = ERB.new(config_template).result(binding)
    put config, "/tmp/my.cnf"
    sudo "mv /tmp/my.cnf #{mysql_config_dir}"
  end

  desc "Create mysql database user and database"
  task :create_database_and_user, :roles => :db do
    config_template = File.read(File.dirname(__FILE__) + "/templates/setup_database.sql.erb");
    config = ERB.new(config_template).result(binding)
    put config, "/tmp/setup_database.sql"
    run "mysql -u root -p#{mysql_root_password} < /tmp/setup_database.sql"
  end

  desc "Creates an Apache 2.2 compatible virtual host configuration file for web server"
  task :create_apache_web_config, :roles => :web do
    config_template = File.read(File.dirname(__FILE__) + "/templates/apache_web.erb");
    config = ERB.new(config_template).result(binding)
    put config, "/tmp/apache-web-#{subdomain}.#{domain}-#{application}.conf"
    sudo "mkdir -p #{apache_web_config_dir}"
    sudo "mv /tmp/apache-web-#{subdomain}.#{domain}-#{application}.conf #{apache_web_config_dir}"
  end

  desc "Create mysql table"
  task :create_table, :roles => :db do
    config_template = File.read(File.dirname(__FILE__) + "/ddl/schema.sql");
    config = ERB.new(config_template).result(binding)
    put config, "/tmp/schema.sql"
    run "mysql -u #{mysql_db_username} -p#{mysql_db_password} #{dbname} < /tmp/schema.sql"
    run "mysql -u #{mysql_db_username} -p#{mysql_db_password} #{dbname}_test < /tmp/schema.sql"
    run "mysql -u #{mysql_db_username} -p#{mysql_db_password} #{dbname}_development < /tmp/schema.sql"
  end

  task :enable_apache_web_config, :roles => :web do
    # sudo "a2ensite apache-web-#{subdomain}.#{domain}-#{application}.conf"
  end

  desc "Creates an Apache 2.2 compatible virtual host configuration file for application server"
  task :create_apache_app_config, :roles => :app do
    config_template = File.read(File.dirname(__FILE__) + "/templates/apache_app.erb");
    config = ERB.new(config_template).result(binding)
    put config, "/tmp/apache-app-#{subdomain}.#{domain}-#{application}.conf"
    sudo "mkdir -p #{apache_app_config_dir}"
    sudo "mv /tmp/apache-app-#{subdomain}.#{domain}-#{application}.conf #{apache_app_config_dir}"
  end

  task :enable_apache_app_config, :roles => :app do
    #sudo "ln -sf #{apache_app_config_dir}/sites-available/apache-app-#{subdomain}.#{domain}-#{application}.conf #{apache_app_config_dir}/sites-enabled/apache-app-#{subdomain}.#{domain}-#{application}.conf"
  end

end
