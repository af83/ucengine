#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Import from the standard library
from json import dumps, loads
from sys import argv, exit, stderr
from urllib2 import Request, urlopen, HTTPError
from urllib import urlencode
from time import time


AUTH_INFOS = [ ('authent', 'anonymous')
             , ('nickname', 'User-%d' % int(time())) # time for (low) unicity
             , ('org', 'af83')
             ]

def send(request):
    """
    Basic utilitary function which send a HTTP request.

        - In case of succes, returns the response body.
        - If an error occurs, the application stops and print the error to
          stderr.
    """
    try:
        response = urlopen(request)
    except HTTPError, error:
        print >> stderr, error
        exit(2)
    return response.read()


def authent(base_url, org):
    """
    Authenticate an anonymous user.

        Returns the session informations (a dict).
    """
    url = '%s/session' % base_url
    data = urlencode(AUTH_INFOS + [('_method', 'PUT')])

    auth_request = Request(url, data)
    session_infos = send(auth_request)
    return loads(session_infos)


def push_event(base_url, org, meeting, euid, esid):
    """
    Push a new_hashtag_event to UCengine.
    """
    url = '%s/event/%s/%s' % (base_url, org, meeting)
    print(url)

    # Create the JSON event
    # This event tells encre to watch a new hashtag on Twitter
    event = dumps({ 'event':
                       { 'type': 'new_hashtag_event'
                       , 'meeting_id': {'org': 'af83', 'meeting': 'demo'}
                       , 'metadatas' : # Metadatas are the only part of the
                                       # event where you can add some specific
                                       # datas.
                                       { 'org': 'af83'
                                       , 'meeting': 'demo'
                                       , 'hashtags': '#encre, #agoroom'
                                       }
                       }
                  # Just for pretty printing
                  }, indent=4)
    print('We sent this event:\n\n%s\n' % event)

    # UrlEncode the data
    # According to the documentation, we need :
    #   - the event (of course)
    #   - the euid and esid of a user
    data = urlencode([ ('event', event)
                     , ('euid', euid)
                     , ('esid', esid)
                     # This parameter allow you to do a PUT request
                     , ('_method', 'PUT')
                     ])

    # Send and retreive
    push_event_request = Request(url, data)
    response = send(push_event_request)
    print('We obtained this response:\n\n%s\n' % response)


def main():
    """
    Usage: push_event.py http://host:port

        The first parameter is the UCengine URL.
        For instance : http://localhost.localdomain:5280
    """
    if len(argv) < 2:
        print >> stderr, 'Bad number of arguments'
        print >> stderr, main.__doc__
        exit(1)

    # Set some default variables
    base_url, org, meeting = argv[1], 'af83', 'demo'

    # We have to authenticate an anonymous user to have access to the UCengine API
    session_infos = authent(base_url, org)['result']
    # Euid and Esid identify the user's session
    euid = session_infos['euid']
    esid = session_infos['esid']

    # We push a custom event
    push_event(base_url, org, meeting, euid, esid)


if __name__ == '__main__':
    main()

