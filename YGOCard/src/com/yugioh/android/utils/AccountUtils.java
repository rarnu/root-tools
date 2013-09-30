package com.yugioh.android.utils;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.content.Context;
import com.yugioh.android.R;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AccountUtils {

    public static String getBindedEmailAddress(Context context) {
        String retStr = "";
        if (context != null) {
            try {
                AccountManager am = AccountManager.get(context);
                Account[] accs = am.getAccounts();
                String google = "";
                String email = "";
                String other = "";
                for (Account a : accs) {
                    if (a.type.equals("com.google")) {
                        google = a.name;
                    } else if (a.type.equals("com.android.email")) {
                        email = a.name;
                    } else {
                        if (isEmail(a.name)) {
                            other = a.name;
                        }
                    }
                }

                if (!google.equals("")) {
                    retStr = google;
                }
                if (retStr.equals("") && !email.equals("")) {
                    retStr = email;
                }
                if (retStr.equals("")) {
                    retStr = other;
                }
                if (retStr.equals("")) {
                    retStr = context.getString(R.string.no_email_binded);
                }
            } catch (Exception e) {

            }
        }
        return retStr;
    }

    private static boolean isEmail(String text) {
        String reg = "^([a-z0-9A-Z]+[-|\\.]?)+[a-z0-9A-Z]@([a-z0-9A-Z]+(-[a-z0-9A-Z]+)?\\.)+[a-zA-Z]{2,}$";
        Pattern regex = Pattern.compile(reg);
        Matcher matcher = regex.matcher(text);
        return matcher.matches();

    }
}
