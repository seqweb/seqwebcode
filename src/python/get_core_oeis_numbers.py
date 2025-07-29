import time
from oeis_session import get_oeis_session


def get_oeis_core_numbers(start=0, step=10, repeat=None, login=True):
    """Get OEIS core numbers using authenticated JSON API.
    
    Args:
        start: Starting offset for the search
        step: Number of results to fetch per request
        repeat: Number of times to repeat the main loop. None means keep looping.
        login: Whether to attempt login. If False, uses unauthenticated requests.
    """
    base_url = "https://oeis.org/search"
    query = "keyword:core"
    fmt = "json"
    start_param = start
    all_numbers = []
    loop_count = 0
    last_results = None  # Track previous results to detect duplicates
    total_count = None  # Will store total count from first response
    
    # Get session (with or without login)
    if login:
        session = get_oeis_session()
        # Follow browser-like navigation pattern
        # print("Following browser-like navigation pattern...")
        session.get('https://oeis.org/')  # Step 1: Visit main page
        # print("Step 1.5: Login (already done by get_oeis_session)")
        session.get('https://oeis.org/search')  # Step 2: Visit search page
    else:
        # Use unauthenticated session
        import requests
        session = requests.Session()
        # Set basic headers for unauthenticated requests
        session.headers.update({
            'User-Agent': ('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) '
                          'AppleWebKit/537.36 (KHTML, like Gecko) '
                          'Chrome/120.0.0.0 Safari/537.36'),
            'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
            'Accept-Language': 'en-US,en;q=0.5',
            'Accept-Encoding': 'gzip, deflate'
        })

    while True:
        # Check repeat limit
        if repeat is not None and loop_count >= repeat:
            print(f"Reached repeat limit of {repeat}. Stopping at start={start_param}")
            break
            
        loop_count += 1
        
        # Force fresh login for requests beyond the limit (only if login=True)
        if login and start_param > 100:
            # print(f"Requesting start={start_param}, forcing fresh login...")
            session = get_oeis_session()
            # Follow browser-like navigation pattern again
            session.get('https://oeis.org/')
            session.get('https://oeis.org/search')
        
        params = {
            "q": query,
            "fmt": fmt,
            "start": start_param
        }
        response = session.get(base_url, params=params)
        
        if response.status_code != 200:
            raise RuntimeError(
                f"Request failed with status {response.status_code}"
            )
        
        # Check if we got HTML instead of JSON
        content_type = response.headers.get('content-type', '')
        if 'text/html' in content_type.lower():
            # print(f"Warning: Received HTML instead of JSON at "
            #       f"start={start_param}")
            # print(f"Content-Type: {content_type}")
            # print(f"Response URL: {response.url}")
            # print(f"Response preview: {response.text[:500]}")
            
            # Try to re-authenticate and retry once (only if login=True)
            if login:
                # print("Attempting to re-authenticate...")
                session = get_oeis_session()
                # Follow browser-like navigation pattern again
                session.get('https://oeis.org/')
                session.get('https://oeis.org/search')
                response = session.get(base_url, params=params)
                
                if response.status_code != 200:
                    raise RuntimeError(
                        f"Request failed after re-authentication: "
                        f"{response.status_code}"
                    )
                
                content_type = response.headers.get('content-type', '')
                if 'text/html' in content_type.lower():
                    error_msg = (f"Still receiving HTML after re-authentication. "
                               f"Content-Type: {content_type}")
                    raise RuntimeError(error_msg)
            else:
                raise RuntimeError(
                    f"Received HTML instead of JSON at start={start_param} "
                    f"(login=False, so no retry attempted)"
                )
        
        try:
            data = response.json()
        except Exception as e:
            print(f"JSON decode error on request {start_param}: {e}")
            print(f"Response text: {response.text[:200]}")
            print(f"Content-Type: {response.headers.get('content-type', 'unknown')}")
            raise

        # Handle case where response is a list instead of dict
        if isinstance(data, list):
            results = data
        else:
            results = data.get("results", [])
        
        # Extract total count from first response
        if total_count is None and not isinstance(data, list):
            total_count = data.get("count", "unknown")
            print(f"Found {total_count} total results")
        
        # Show progress indicator
        print(".", end="", flush=True)
        
        # Check for duplicate results (OEIS returns last page when start > total)
        if last_results is not None and len(results) == len(last_results):
            # Compare first and last results to see if they're the same
            if (results[0].get('number') == last_results[0].get('number') and 
                results[-1].get('number') == last_results[-1].get('number')):
                print()  # New line after progress dots
                break
        
        last_results = results.copy()
            
        if not results:
            print()  # New line after progress dots
            break

        all_numbers.extend(entry["number"] for entry in results)
        start_param += step
        time.sleep(0.1)  # Be polite to OEIS servers

    return sorted(all_numbers) 