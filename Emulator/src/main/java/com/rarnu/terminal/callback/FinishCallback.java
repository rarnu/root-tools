package com.rarnu.terminal.callback;

import com.rarnu.terminal.session.TermSession;

public interface FinishCallback {
    void onSessionFinish(TermSession session);
}