FROM container-registry.oracle.com/database/free:latest

COPY setup/ /opt/oracle/scripts/setup/
COPY entrypoint.sh /opt/oracle/entrypoint.sh
USER root
RUN chmod +x /opt/oracle/entrypoint.sh
USER oracle

ENTRYPOINT ["/opt/oracle/entrypoint.sh"]
