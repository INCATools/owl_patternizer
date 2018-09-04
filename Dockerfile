FROM cmungall/sparqlprog
MAINTAINER Chris Mungall <cjmungall@lbl.gov>

RUN swipl -g "Opts=[interactive(false)],pack_install(index_util,Opts),pack_install(sparqlprog,Opts),halt"
ENV PATH "/tools/bin:$PATH"

CMD swipl -p library=prolog ./bin/rdfmatch -h
