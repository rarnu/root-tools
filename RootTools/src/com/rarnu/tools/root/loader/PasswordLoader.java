package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.PasswordItem;
import com.rarnu.tools.root.database.PasswordCaller;

import java.util.List;

public class PasswordLoader extends BaseLoader<PasswordItem> {

    public PasswordLoader(Context context) {
        super(context);
    }

    @Override
    public List<PasswordItem> loadInBackground() {
        return PasswordCaller.getPasswordList(getContext());
    }
}
