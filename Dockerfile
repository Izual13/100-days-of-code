FROM bellsoft/liberica-openjdk-centos

RUN rpm -Uvh https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm

RUN yum install -y \
       rlwrap \ 
    && curl -O https://download.clojure.org/install/linux-install-1.10.3.1040.sh \
    && chmod +x linux-install-1.10.3.1040.sh \
    && ./linux-install-1.10.3.1040.sh \
    && yum clean all

EXPOSE 5000

COPY ./resources/ ./resources/
COPY ./deps.edn ./deps.edn

CMD clj -M:repl


