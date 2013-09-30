package com.yugioh.android.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseClassLoader;
import com.yugioh.android.utils.YGOAPI;

public class FeedbackSender extends BaseClassLoader<Boolean> {
    private String text;

    public FeedbackSender(Context context) {
        super(context);
    }

    public void setText(String text) {
        this.text = text;
    }

    @Override
    public Boolean loadInBackground() {
        return YGOAPI.sendFeedback(getContext(), text);
    }
}
