FROM ageron/handson-ml
USER root
COPY /requirements.txt /tmp/
RUN pip install -r /tmp/requirements.txt
RUN jupyter contrib nbextension install