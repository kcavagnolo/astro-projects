     define is_file (file)
     {
        variable st;

        st = stat_file (file);
        if (st == NULL) return 0;
        else return 1;
     }


