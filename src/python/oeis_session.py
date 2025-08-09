import os
import requests
import re
# import time


class OEISSession:
    """Manages authenticated OEIS session with login and cookie persistence."""
    
    def __init__(self):
        self.session = requests.Session()
        self.authenticated = False
        
        # Browser-like headers
        self.session.headers.update({
            'User-Agent': ('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) '
                          'AppleWebKit/537.36 (KHTML, like Gecko) '
                          'Chrome/120.0.0.0 Safari/537.36'),
            'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
            'Accept-Language': 'en-US,en;q=0.5',
            'Accept-Encoding': 'gzip, deflate',
            'DNT': '1',
            'Connection': 'keep-alive',
            'Upgrade-Insecure-Requests': '1',
            'Sec-Fetch-Dest': 'document',
            'Sec-Fetch-Mode': 'navigate',
            'Sec-Fetch-Site': 'same-origin',
            'Sec-Fetch-User': '?1',
            'Cache-Control': 'max-age=0'
        })
    
    def _extract_hidden_fields(self, html_content):
        """Extract hidden form fields using regex instead of BeautifulSoup.
        See @https://blog.codinghorror.com/content/images/2014/Apr/stack-overflow-regex-zalgo.png
        """
        hidden_fields = {}
        
        # Pattern to match hidden input fields
        pattern = r'<input[^>]*type=["\']hidden["\'][^>]*>'
        hidden_inputs = re.findall(pattern, html_content, re.IGNORECASE)
        
        for input_tag in hidden_inputs:
            # Extract name and value attributes
            name_match = re.search(r'name=["\']([^"\']*)["\']', input_tag)
            value_match = re.search(r'value=["\']([^"\']*)["\']', input_tag)
            
            if name_match:
                name = name_match.group(1)
                value = value_match.group(1) if value_match else ''
                hidden_fields[name] = value
        
        return hidden_fields
    
    def login(self):
        """Login to OEIS using credentials from environment variables."""
        username = os.getenv('OEIS_USERNAME')
        password = os.getenv('OEIS_PASSWORD')
        # password = "!!KNOWN_WRONG_PASSWORD"
        
        if not username or not password:
            raise ValueError("OEIS_USERNAME and OEIS_PASSWORD environment variables must be set")
        
        # First visit the login page to get any necessary tokens
        login_url = "https://oeis.org/login"
        response = self.session.get(login_url)
        
        if response.status_code != 200:
            raise RuntimeError(f"Failed to access login page: {response.status_code}")
        
        # print(f"Initial cookies: {dict(self.session.cookies)}")
        
        # Extract hidden fields using regex
        hidden_fields = self._extract_hidden_fields(response.text)
        # print(f"Found hidden fields: {hidden_fields}")
        
        # Debug: show the actual form structure
        form_match = re.search(r'<form[^>]*method=["\']POST["\'][^>]*>.*?</form>', response.text, re.DOTALL | re.IGNORECASE)
        # if form_match:
        #     print(f"Login form found: {form_match.group(0)[:200]}...")
        # else:
        #     print("No POST form found in response")
        
        # Look for JavaScript or dynamic content
        # if 'script' in response.text.lower():
        #     print("Found JavaScript in response")
        # if 'iframe' in response.text.lower():
        #     print("Found iframe in response")
        # if 'popup' in response.text.lower() or 'modal' in response.text.lower():
        #     print("Found popup/modal references in response")
        
        # Prepare login data
        login_data = {
            'username': username,
            'password': password,
            'remember': 'on'  # Remember me checkbox
        }
        
        # Add any hidden fields from the form
        login_data.update(hidden_fields)
        # print(f"Login data keys: {list(login_data.keys())}")
        # print(f"Login data values: {dict(login_data)}")
        
        # Submit login form
        login_url = "https://oeis.org/login"
        
        # Add more browser-like headers for the login request
        headers = {
            'Referer': 'https://oeis.org/login',
            'Origin': 'https://oeis.org',
            'Content-Type': 'application/x-www-form-urlencoded'
        }
        
        response = self.session.post(login_url, data=login_data, 
                                   headers=headers, allow_redirects=False)
        
        # print(f"Login POST status: {response.status_code}")
        # print(f"Login POST URL: {response.url}")
        # print(f"Login POST headers: {dict(response.headers)}")
        
        # Check if we got a redirect (301 = successful login)
        if response.status_code == 301:
            redirect_url = response.headers.get('Location')
            # print(f"Got 301 redirect to: {redirect_url}")
            
            # Make redirect URL absolute if it's relative
            if redirect_url.startswith('/'):
                redirect_url = 'https://oeis.org' + redirect_url
                # print(f"Made redirect URL absolute: {redirect_url}")
            
            # Follow the redirect to the main page
            response = self.session.get(redirect_url, allow_redirects=True)
            # print(f"After redirect status: {response.status_code}")
            # print(f"After redirect URL: {response.url}")
        
        if response.status_code != 200:
            raise RuntimeError(f"Login failed with status {response.status_code}")
        
        # print(f"After login cookies: {dict(self.session.cookies)}")
        # print(f"Login response URL: {response.url}")
        # print(f"Login response headers: {dict(response.headers)}")
        # print(f"Login response contains 'Set-Cookie': "
        #       f"{'Set-Cookie' in response.headers}")
        
        # Check the response content for error messages
        # print(f"Login response content preview: {response.text[:1000]}")
        
        # Check for specific error messages
        # if 'error' in response.text.lower() or 'invalid' in response.text.lower():
        #     print("Found potential error messages in response")
        
        # Check if login was successful by looking for logout link on the main page
        if 'href="/logout' in response.text or 'https://oeis.org/logout' in response.text:
            self.authenticated = True
            print("Successfully logged into OEIS")
        else:
            # print(f"Login failed - no logout link found. "
            #       f"Response URL: {response.url}")
            # print(f"Full response length: {len(response.text)}")
            # Show where logout might be
            # logout_pos = response.text.find('logout')
            # if logout_pos != -1:
            #     print(f"Found 'logout' at position {logout_pos}: {response.text[logout_pos-50:logout_pos+50]}")
            raise RuntimeError("Login failed - no logout link found")
    
    def get(self, url, params=None):
        """Make an authenticated GET request to OEIS."""
        if not self.authenticated:
            self.login()
        
        # Set Referer to make request look more browser-like
        if 'oeis.org' in url:
            self.session.headers['Referer'] = 'https://oeis.org/'
        
        response = self.session.get(url, params=params)
        return response
    
    def post(self, url, data=None):
        """Make an authenticated POST request to OEIS."""
        if not self.authenticated:
            self.login()
        
        response = self.session.post(url, data=data)
        return response


# Global session instance for reuse across functions
_oeis_session = None


def get_oeis_session():
    """Get or create the global OEIS session."""
    global _oeis_session
    if _oeis_session is None:
        _oeis_session = OEISSession()
    return _oeis_session
