{"nbformat":4,"nbformat_minor":0,"metadata":{"colab":{"name":"pyidioms.py","provenance":[],"authorship_tag":"ABX9TyNRi6+AsGdATiByq73pXJV2"},"kernelspec":{"name":"python3","display_name":"Python 3"}},"cells":[{"cell_type":"code","metadata":{"id":"zVRoumE9jP05","colab_type":"code","colab":{"base_uri":"https://localhost:8080/","height":408},"executionInfo":{"status":"error","timestamp":1600320872093,"user_tz":420,"elapsed":1060,"user":{"displayName":"Supriyo Roy Banerjee","photoUrl":"https://lh3.googleusercontent.com/a-/AOh14GjWwpJR1TgHfhyh2VxVbzMqgMUc3hrJUzjM96IH9w=s64","userId":"07613904816325028652"}},"outputId":"e8d5d381-2415-4776-d92b-67756542ccc5"},"source":["import pandas as pd\n","df = pd.read_csv('census.csv')\n","df"],"execution_count":1,"outputs":[{"output_type":"error","ename":"FileNotFoundError","evalue":"ignored","traceback":["\u001b[0;31m---------------------------------------------------------------------------\u001b[0m","\u001b[0;31mFileNotFoundError\u001b[0m                         Traceback (most recent call last)","\u001b[0;32m<ipython-input-1-0a5244af99b7>\u001b[0m in \u001b[0;36m<module>\u001b[0;34m()\u001b[0m\n\u001b[1;32m      1\u001b[0m \u001b[0;32mimport\u001b[0m \u001b[0mpandas\u001b[0m \u001b[0;32mas\u001b[0m \u001b[0mpd\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m----> 2\u001b[0;31m \u001b[0mdf\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mpd\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mread_csv\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0;34m'census.csv'\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m      3\u001b[0m \u001b[0mdf\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n","\u001b[0;32m/usr/local/lib/python3.6/dist-packages/pandas/io/parsers.py\u001b[0m in \u001b[0;36mparser_f\u001b[0;34m(filepath_or_buffer, sep, delimiter, header, names, index_col, usecols, squeeze, prefix, mangle_dupe_cols, dtype, engine, converters, true_values, false_values, skipinitialspace, skiprows, skipfooter, nrows, na_values, keep_default_na, na_filter, verbose, skip_blank_lines, parse_dates, infer_datetime_format, keep_date_col, date_parser, dayfirst, cache_dates, iterator, chunksize, compression, thousands, decimal, lineterminator, quotechar, quoting, doublequote, escapechar, comment, encoding, dialect, error_bad_lines, warn_bad_lines, delim_whitespace, low_memory, memory_map, float_precision)\u001b[0m\n\u001b[1;32m    674\u001b[0m         )\n\u001b[1;32m    675\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 676\u001b[0;31m         \u001b[0;32mreturn\u001b[0m \u001b[0m_read\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mfilepath_or_buffer\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mkwds\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m    677\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    678\u001b[0m     \u001b[0mparser_f\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0m__name__\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mname\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n","\u001b[0;32m/usr/local/lib/python3.6/dist-packages/pandas/io/parsers.py\u001b[0m in \u001b[0;36m_read\u001b[0;34m(filepath_or_buffer, kwds)\u001b[0m\n\u001b[1;32m    446\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    447\u001b[0m     \u001b[0;31m# Create the parser.\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 448\u001b[0;31m     \u001b[0mparser\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mTextFileReader\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mfp_or_buf\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0;34m**\u001b[0m\u001b[0mkwds\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m    449\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    450\u001b[0m     \u001b[0;32mif\u001b[0m \u001b[0mchunksize\u001b[0m \u001b[0;32mor\u001b[0m \u001b[0miterator\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n","\u001b[0;32m/usr/local/lib/python3.6/dist-packages/pandas/io/parsers.py\u001b[0m in \u001b[0;36m__init__\u001b[0;34m(self, f, engine, **kwds)\u001b[0m\n\u001b[1;32m    878\u001b[0m             \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0moptions\u001b[0m\u001b[0;34m[\u001b[0m\u001b[0;34m\"has_index_names\"\u001b[0m\u001b[0;34m]\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mkwds\u001b[0m\u001b[0;34m[\u001b[0m\u001b[0;34m\"has_index_names\"\u001b[0m\u001b[0;34m]\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    879\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 880\u001b[0;31m         \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0m_make_engine\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mengine\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m    881\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    882\u001b[0m     \u001b[0;32mdef\u001b[0m \u001b[0mclose\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n","\u001b[0;32m/usr/local/lib/python3.6/dist-packages/pandas/io/parsers.py\u001b[0m in \u001b[0;36m_make_engine\u001b[0;34m(self, engine)\u001b[0m\n\u001b[1;32m   1112\u001b[0m     \u001b[0;32mdef\u001b[0m \u001b[0m_make_engine\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mengine\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0;34m\"c\"\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m   1113\u001b[0m         \u001b[0;32mif\u001b[0m \u001b[0mengine\u001b[0m \u001b[0;34m==\u001b[0m \u001b[0;34m\"c\"\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m-> 1114\u001b[0;31m             \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0m_engine\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mCParserWrapper\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mf\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0;34m**\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0moptions\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m   1115\u001b[0m         \u001b[0;32melse\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m   1116\u001b[0m             \u001b[0;32mif\u001b[0m \u001b[0mengine\u001b[0m \u001b[0;34m==\u001b[0m \u001b[0;34m\"python\"\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n","\u001b[0;32m/usr/local/lib/python3.6/dist-packages/pandas/io/parsers.py\u001b[0m in \u001b[0;36m__init__\u001b[0;34m(self, src, **kwds)\u001b[0m\n\u001b[1;32m   1889\u001b[0m         \u001b[0mkwds\u001b[0m\u001b[0;34m[\u001b[0m\u001b[0;34m\"usecols\"\u001b[0m\u001b[0;34m]\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0musecols\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m   1890\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m-> 1891\u001b[0;31m         \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0m_reader\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mparsers\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mTextReader\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0msrc\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0;34m**\u001b[0m\u001b[0mkwds\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m   1892\u001b[0m         \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0munnamed_cols\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0m_reader\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0munnamed_cols\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m   1893\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n","\u001b[0;32mpandas/_libs/parsers.pyx\u001b[0m in \u001b[0;36mpandas._libs.parsers.TextReader.__cinit__\u001b[0;34m()\u001b[0m\n","\u001b[0;32mpandas/_libs/parsers.pyx\u001b[0m in \u001b[0;36mpandas._libs.parsers.TextReader._setup_parser_source\u001b[0;34m()\u001b[0m\n","\u001b[0;31mFileNotFoundError\u001b[0m: [Errno 2] File census.csv does not exist: 'census.csv'"]}]}]}