package com.rarnu.devlib.base;

import android.content.AsyncTaskLoader;
import android.content.Context;
import android.database.Cursor;

public abstract class BaseCursorLoader extends AsyncTaskLoader<Cursor> {

    public BaseCursorLoader(Context context) {
        super(context);
    }

    public abstract Cursor loadInBackground();

    @Override
    protected void onStartLoading() {
        forceLoad();
    }

    @Override
    public void onCanceled(Cursor data) {
        super.onCanceled(data);
    }

    @Override
    protected void onStopLoading() {
        cancelLoad();
    }

    @Override
    protected void onReset() {
        stopLoading();

    }
}
